{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Operations
    ( Model.Operations.getOperatorId
    , Model.Operations.addAdmin
    , Model.Operations.rmAdmin
    , getChanName
    , getChanDesc
    , setChanName
    , setChanDesc
    , addChanOwner
    , addChanProducer
    , addChanConsumer
    , rmChanOwner
    , rmChanProducer
    , rmChanConsumer
    , subscribeToChan
    , unsubscribeFromChan
    , getUserByLogin
    , getUserLogin
    , getUserName
    , getUserDesc
    , getUserIcon
    , getUserOwnedChannels
    , getUserSubscriptions
    , getUserContacts
    , addUserContact
    , rmUserContact
    , setUserLogin
    , setUserPassword
    , setUserName
    , setUserDesc
    , setUserIcon
    , createUser
    , createChannel
    , post
    , Model.Operations.messages
    , Offset
    , Amount
    )
where

import qualified Data.Set as S
import qualified Data.IxSet as IX
import Data.Text hiding (drop, take)
import Control.Lens
import Control.Lens.Prism
import Control.Applicative ((<$>))
import Data.Time.Clock (UTCTime)
import Data.Maybe (isJust)

import Model.Errors
import Model.Permissions
import Model.BaseTypes
import Model.OpMonad
import Model.NoC
import qualified Model.NoC as N
import qualified Model.Channel as C
import Model.Channel
import qualified Model.User as U
import Model.User 
import qualified Model.Message as M
import Model.Message 

type SimpleLens a b = Lens a a b b 

---------
-- on noc
---------

getOperatorId :: OpMonad m => m UserId
getOperatorId = ifIsLoggedIn' return 

addAdmin :: OpMonad m => UserId -> m ()
addAdmin uid = checkAccess () forNoCAdmins $ do
    getUser uid
    Model.OpMonad.addAdmin uid

rmAdmin :: OpMonad m => UserId -> m ()
rmAdmin uid = checkAccess () forNoCAdmins $ do
    n <- fmap S.size getAdmins
    OnlyOneNoCAdminLeft `throwOn` (n == 1) 
    Model.OpMonad.rmAdmin uid

-------------------------
-- operations on channels
-------------------------

getFromChan :: OpMonad m => SimpleLens Channel a -> ChanId -> m a
getFromChan l cid = checkAccess cid forAllChanPeople $ do
    chan <- getChannel cid
    return $ chan ^. l

setToChan :: OpMonad m => SimpleLens Channel a -> ChanId -> a -> m ()
setToChan l cid v = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- getChannel cid
    storeChannel (set l v chan)  

getChanName cid = getFromChan C.name cid
getChanDesc cid = getFromChan C.desc cid

setChanName cid v = setToChan C.name cid v
setChanDesc cid v = setToChan C.desc cid v

addChanXX :: OpMonad m 
          => SimpleLens Channel (S.Set UserId)
          -> ChanId -> UserId -> m ()
addChanXX l cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- getChannel cid
    storeChannel (over l (S.insert uid) chan)

rmChanXX :: OpMonad m 
         => SimpleLens Channel (S.Set UserId)
         -> ChanId -> UserId -> m ()
rmChanXX l cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- getChannel cid
    storeChannel (over l (S.delete uid) chan)

addChanOwner cid uid = addChanXX C.owners cid uid
addChanProducer cid uid = addChanXX C.producers cid uid
addChanConsumer cid uid = addChanXX C.consumers cid uid

rmChanOwner cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- getChannel cid
    OnlyOneChannelOwnerLeft cid `throwOn` (S.size (chan ^. owners) == 0)
    storeChannel (over C.owners (S.delete uid) chan)
rmChanProducer cid uid = rmChanXX C.producers cid uid
rmChanConsumer cid uid = rmChanXX C.consumers cid uid

subscribeToChan :: OpMonad m => UserId -> ChanId -> m () 
subscribeToChan uid cid = checkAccess cid forAllChanPeople $
    checkAccess uid forUserSelfOrAdmins $ do
        user <- getUser uid
        storeUser (over U.subscriptions (S.insert cid) user)  
      
unsubscribeFromChan :: OpMonad m => UserId -> ChanId -> m ()
unsubscribeFromChan uid cid = checkAccess uid forUserSelfOrAdmins $ do
        user <- getUser uid
        storeUser (over U.subscriptions (S.delete cid) user)

----------------------
-- operations on users
----------------------

getFromUser :: OpMonad m => SimpleLens User a -> UserId -> m a
getFromUser l uid = ifIsLoggedIn $ do
    user <- getUser uid
    return $ user ^. l 

setToUser :: OpMonad m => SimpleLens User a -> UserId -> a -> m ()
setToUser l uid v = checkAccess uid forUserSelfOrAdmins $ do
    user <- getUser uid
    storeUser (set l v user) 

overInUser :: OpMonad m => SimpleLens User a -> UserId -> (a -> a) -> m ()
overInUser l uid fun = checkAccess uid forUserSelfOrAdmins $ do
    user <- getUser uid
    storeUser (over l fun user)

getUserLogin uid = getFromUser U.login uid
getUserName uid = getFromUser U.name uid
getUserDesc uid = getFromUser U.desc uid
getUserIcon uid = getFromUser U.icon uid
getUserOwnedChannels uid = checkAccess uid forUserSelfOrAdmins
    $ getFromUser U.ownedChannels uid
getUserSubscriptions uid = checkAccess uid forUserSelfOrAdmins
    $ getFromUser U.subscriptions uid
getUserContacts uid = checkAccess uid forUserSelfOrAdmins
    $ getFromUser U.contacts uid

addUserContact uid other = overInUser U.contacts uid (S.insert other)  
rmUserContact uid other = overInUser U.contacts uid (S.delete other)  

setUserLogin uid l = do
    oid <- Model.Operations.getOperatorId
    l' <- getUserLogin oid
    if uid == oid && l == l'
        then return ()
        else do
            checkDuplicateLogin l
            setToUser U.login uid l
setUserName uid v = setToUser U.name uid v
setUserPassword uid v = setToUser U.password uid v
setUserDesc uid v = setToUser U.desc uid v
setUserIcon uid v = setToUser U.icon uid v

getUserByLogin :: OpMonad m => Text -> m UserId
getUserByLogin l = ifIsLoggedIn $ do
    user <- IX.getOne <$> getUsers @= (Login l)
    case user of
        (Just user) -> return $ U._id user
        otherwise -> throw $ UnknownLogin l 

--------------------
-- creation of users
--------------------

createUser :: OpMonad m => Login -> Password -> m UserId
createUser l pw = checkAccess () forNoCAdmins $ do
    uid <- newUserId
    storeUser $ User uid l pw (mkName "") (mkDesc "") Nothing S.empty S.empty S.empty
    return uid

-----------------------
-- creation of channels
-----------------------

createChannel :: OpMonad m => Name -> Desc -> m ChanId
createChannel n d = ifIsLoggedIn $ do
    cid <- newChanId
    oid <- Model.Operations.getOperatorId
    user <- getUser oid
    storeChannel $ Channel cid n d (S.insert oid S.empty) S.empty S.empty S.empty
    storeUser $ over U.ownedChannels (S.insert cid) user
    return cid

----------------------
-- posting of messages
----------------------

post :: OpMonad m => ChanId -> UTCTime -> Text -> Maybe Image -> m MsgId
post cid ts txt img = checkAccess cid forChanProducers $ do
    mid <- newMsgId 
    oid <- Model.Operations.getOperatorId
    chan <- getChannel cid
    storeChannel $ over C.messages (S.insert mid) chan
    storeMessage $ Message mid cid img txt oid ts
    return mid

----------------------
-- reading of messages
----------------------

type Offset = Int
type Amount = Int

messages :: OpMonad m => ChanId -> Offset -> Amount -> m [Message]
messages cid ofs am = checkAccess cid forConsumersOrOwners $ do
    take am . drop ofs . IX.toDescList (IX.Proxy :: IX.Proxy UTCTime) <$> getMessages @= cid  
    
----------
-- helpers
----------

checkDuplicateLogin l = do
    n <- IX.size <$> getUsers @= l 
    DuplicateLogin l `throwOn` (n >= 1) 
