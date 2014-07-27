{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Operations
    ( runOp
    , addAdmin
    , rmAdmin
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
    , getUserLogin
    , getUserName
    , getUserDesc
    , getUserIcon
    , getUserOwnedChannels
    , getUserSubscriptions
    , getUserContacts
    , setUserLogin
    , setUserName
    , setUserDesc
    , setUserIcon
    , createUser
    , createChannel
    , post
    )
where

import qualified Data.Set as S
import qualified Data.IxSet as IX
import Data.Text
import Control.Lens
import Control.Applicative ((<$>))
import Data.Time.Clock (UTCTime)

import Model.Errors
import Model.Permissions
import Model.BaseTypes
import Model.OpMonad
import qualified Model.UnsafeOperations as US
import Model.NoC
import qualified Model.NoC as N
import qualified Model.Channel as C
import Model.Channel
import qualified Model.User as U
import Model.User 
import qualified Model.Message as M
import Model.Message 

type SimpleLens a b = Lens a a b b 
a @= b = a US.@= b

-- on noc

logUserIn :: NoC -> Login -> Password -> Maybe UserId
logUserIn noc l pw = do
    user <- IX.getOne (N._users noc IX.@= l)
    if checkPassword (U._password user) pw
        then return (U._id user)
        else fail "password mismatch" 

runOp :: NoC -> Login -> Password -> Operation a -> Either Error (NoC, a)
runOp noc l pw op = 
    case oid' of
        Nothing -> Left $ CantLogin l pw
        otherwise -> case res of
            Left err -> Left err
            Right (cont, v) -> Right (_noc cont, v)
    where
    res = runOp' noc oid op
    (Just oid) = oid'
    oid' = logUserIn noc l pw 

addAdmin :: UserId -> Operation ()
addAdmin = checkAccess () forNoCAdmins . US.addAdmin 

rmAdmin :: UserId -> Operation ()
rmAdmin uid = checkAccess () forNoCAdmins $ do
    n <- fmap S.size US.getAdmins
    OnlyOneNoCAdminLeft `throwOn` (n == 1) 
    US.rmAdmin uid


-- operations on channels

getFromChan :: SimpleLens Channel a -> ChanId -> Operation a
getFromChan l cid = checkAccess cid forAllChanPeople $ do
    chan <- US.getChannel cid
    return $ chan ^. l

setToChan :: SimpleLens Channel a -> ChanId -> a -> Operation ()
setToChan l cid v = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    US.storeChannel (set l v chan)  

getChanName = getFromChan C.name
getChanDesc = getFromChan C.desc

setChanName = setToChan C.name
setChanDesc = setToChan C.desc

addChanXX :: SimpleLens Channel (S.Set UserId)
          -> ChanId -> UserId -> Operation ()
addChanXX l cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    US.storeChannel (over l (S.insert uid) chan)

rmChanXX :: SimpleLens Channel (S.Set UserId)
         -> ChanId -> UserId -> Operation ()
rmChanXX l cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    US.storeChannel (over l (S.delete uid) chan)

addChanOwner = addChanXX C.owners
addChanProducer = addChanXX C.producers
addChanConsumer = addChanXX C.consumers

rmChanOwner cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    OnlyOneChannelOwnerLeft cid `throwOn` (S.size (chan ^. owners) == 0)
    US.storeChannel (over C.owners (S.delete uid) chan)
rmChanProducer = rmChanXX C.producers
rmChanConsumer = rmChanXX C.consumers


-- operations on users

getFromUser :: SimpleLens User a
            -> UserId -> Operation a
getFromUser l uid = do
    user <- US.getUser uid
    return $ user ^. l 

setToUser :: SimpleLens User a
          -> UserId -> a -> Operation ()
setToUser l uid v = checkAccess uid forUserSelfOrAdmins $ do
    user <- US.getUser uid
    US.storeUser (set l v user) 

getUserLogin = getFromUser U.login
getUserName = getFromUser U.name
getUserDesc = getFromUser U.desc
getUserIcon = getFromUser U.icon
getUserOwnedChannels uid = checkAccess uid forUserSelfOrAdmins
    $ getFromUser U.ownedChannels uid
getUserSubscriptions uid = checkAccess uid forUserSelfOrAdmins
    $ getFromUser U.subscriptions uid
getUserContacts uid = checkAccess uid forUserSelfOrAdmins
    $ getFromUser U.contacts uid

setUserLogin uid l = do
    checkDuplicateLogin l
    setToUser U.login uid l
setUserName = setToUser U.name
setUserDesc = setToUser U.desc
setUserIcon = setToUser U.icon


-- creation of users

createUser :: Login -> Password -> Operation UserId
createUser l pw = checkAccess () forNoCAdmins $ do
    uid <- US.newUserId
    US.storeUser $ User uid l pw (Name "") (Desc "") Nothing S.empty S.empty S.empty
    return uid

-- creation of channels

createChannel :: Name -> Desc -> Operation ChanId
createChannel n d = do
    cid <- US.newChanId
    oid <- US.getOperatorId
    user <- US.getUser oid
    US.storeChannel $ Channel cid n d (S.insert oid S.empty) S.empty S.empty S.empty
    US.storeUser $ over U.ownedChannels (S.insert cid) user
    return cid

-- posting of messages

post :: ChanId -> UTCTime -> Text -> Maybe Image -> Operation MsgId
post cid ts txt img = checkAccess cid forChanProducers $ do
    mid <- US.newMsgId 
    oid <- US.getOperatorId
    chan <- US.getChannel cid
    US.storeChannel $ over C.messages (S.insert mid) chan
    US.storeMessage $ Message mid img txt oid ts
    return mid
    

-- helpers

checkDuplicateLogin l = do
    n <- IX.size <$> US.getUsers @= l 
    DuplicateLogin l `throwOn` (n >= 1) 
