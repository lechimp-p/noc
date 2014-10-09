{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.Operations 
    ( module Model.Operations
    , module Control.Eff 
    , E.Exec
    , Q.Query
    , U.Update
    , Model.Permissions.forceOperatorId
    )
where

import Model.BaseTypes
import Model.Errors
import Model.Permissions
import Model.Exec (Exec)
import Model.Query (Query)
import Model.Update (Update)
import qualified Model.Exec as E
import qualified Model.Query as Q
import qualified Model.Update as U
import Model.Message 

import qualified Data.Set as S
import Data.Text (Text) 
import Data.Time.Clock (UTCTime)
import Data.Maybe (isJust)
import Control.Eff

throwOn err cond =
    if cond
    then E.throwME err
    else return ()

---------
-- on noc
---------

isAdmin :: (Member Query r, Member Exec r)
        => UserId -> Eff r Bool
isAdmin uid = do
    checkAccess () forAdmins 
    Q.isAdmin uid 

addAdmin :: (Member Update r, Member Query r, Member Exec r) 
         => UserId -> Eff r ()
addAdmin uid = do
    checkAccess () forAdmins 
    getUserLogin uid -- to raise if user not exists
    U.addAdmin uid

rmAdmin :: (Member Update r, Member Query r, Member Exec r) 
        => UserId -> Eff r ()
rmAdmin uid = do
    checkAccess () forAdmins 
    n <- Q.countAdmins 
    OnlyOneNoCAdminLeft `throwOn` (n == 1) 
    U.rmAdmin uid

------------
-- execution
------------

getOperatorId :: (Member Exec r)
              => Eff r (Maybe UserId)
getOperatorId = E.getOperatorId 

doLogin :: (Member Exec r)
        => Login -> Password -> Eff r UserId
doLogin l pw = do
    oid <- getOperatorId
    AlreadyLoggedIn `throwOn` (isJust oid)
    E.doLogin l pw

doLogout :: (Member Exec r)
         => Eff r ()
doLogout = E.doLogout 

-------------------------
-- operations on channels
-------------------------

getChanName :: (Member Query r, Member Exec r)
            => ChanId -> Eff r Name
getChanName cid = do
    checkAccess cid forAllChanPeople
    Q.getChanName cid

getChanDesc :: (Member Query r, Member Exec r)
            => ChanId -> Eff r Desc 
getChanDesc cid = do
    checkAccess cid forAllChanPeople
    Q.getChanDesc cid

getChanType :: (Member Query r, Member Exec r)
            => ChanId -> Eff r ChanType 
getChanType cid = do
    checkAccess cid forAllChanPeople
    Q.getChanType cid

setChanName :: (Member Update r, Member Query r, Member Exec r)
            => ChanId -> Name -> Eff r () 
setChanName cid v = do
    checkAccess cid forChanOwnersOrAdmins
    U.setChanName cid v 

setChanDesc :: (Member Update r, Member Query r, Member Exec r)
            => ChanId -> Desc -> Eff r () 
setChanDesc cid v = do
    checkAccess cid forChanOwnersOrAdmins
    U.setChanDesc cid v 

setChanType :: (Member Update r, Member Query r, Member Exec r)
            => ChanId -> ChanType -> Eff r () 
setChanType cid v = do
    checkAccess cid forChanOwnersOrAdmins
    U.setChanType cid v 

isChanOwner :: (Member Query r, Member Exec r)
            => ChanId -> UserId -> Eff r Bool
isChanOwner cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    Q.isChanOwner cid uid

addChanOwner :: (Member Update r, Member Query r, Member Exec r)
             => ChanId -> UserId -> Eff r ()
addChanOwner cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    U.addChanOwner cid uid

rmChanOwner :: (Member Update r, Member Query r, Member Exec r)
            => ChanId -> UserId -> Eff r ()
rmChanOwner cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    U.rmChanOwner cid uid

isChanProducer :: (Member Query r, Member Exec r)
            => ChanId -> UserId -> Eff r Bool
isChanProducer cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    Q.isChanProducer cid uid

addChanProducer :: (Member Update r, Member Query r, Member Exec r)
             => ChanId -> UserId -> Eff r ()
addChanProducer cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    U.addChanProducer cid uid

rmChanProducer :: (Member Update r, Member Query r, Member Exec r)
            => ChanId -> UserId -> Eff r ()
rmChanProducer cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    U.rmChanProducer cid uid

isChanConsumer :: (Member Query r, Member Exec r)
            => ChanId -> UserId -> Eff r Bool
isChanConsumer cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    Q.isChanConsumer cid uid

addChanConsumer :: (Member Update r, Member Query r, Member Exec r)
             => ChanId -> UserId -> Eff r ()
addChanConsumer cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    U.addChanConsumer cid uid

rmChanConsumer :: (Member Update r, Member Query r, Member Exec r)
            => ChanId -> UserId -> Eff r ()
rmChanConsumer cid uid = do
    checkAccess cid forChanOwnersOrAdmins
    U.rmChanConsumer cid uid

amountOfSubscribedUsers :: (Member Query r, Member Exec r)
                        => ChanId -> Eff r Int
amountOfSubscribedUsers cid = do
    checkAccess cid forAllChanPeople 
    Q.amountOfSubscribedUsers cid

lastPostTimestamp :: (Member Query r, Member Exec r)
                  => ChanId -> Eff r (Maybe UTCTime)
lastPostTimestamp cid = do
    checkAccess cid forAllChanPeople
    Q.lastPostTimestamp cid

subscribeToChan :: (Member Update r, Member Query r, Member Exec r)
                => UserId -> ChanId -> Eff r () 
subscribeToChan uid cid = do
    checkAccess cid forAllChanPeople 
    checkAccess uid forUserSelfOrAdmins 
    U.addUserSubscription uid cid
      
unsubscribeFromChan :: (Member Update r, Member Query r, Member Exec r) 
                    => UserId -> ChanId -> Eff r ()
unsubscribeFromChan uid cid = do
        checkAccess uid forUserSelfOrAdmins 
        U.rmUserSubscription uid cid

----------------------
-- operations on users
----------------------

searchUserByLogin :: (Member Query r, Member Exec r)
                  => Text -> Eff r (S.Set UserId)
searchUserByLogin l = do
    forceOperatorId
    Q.searchUserByLogin l

getUserLogin :: (Member Query r, Member Exec r)
             => UserId -> Eff r Login 
getUserLogin uid = do
    forceOperatorId
    Q.getUserLogin uid

getUserName :: (Member Query r, Member Exec r)
             => UserId -> Eff r Name 
getUserName uid = do
    forceOperatorId
    Q.getUserName uid

getUserDesc :: (Member Query r, Member Exec r)
             => UserId -> Eff r Desc 
getUserDesc uid = do
    forceOperatorId
    Q.getUserDesc uid

getUserIcon :: (Member Query r, Member Exec r)
             => UserId -> Eff r (Maybe Icon) 
getUserIcon uid = do
    forceOperatorId
    Q.getUserIcon uid

getUserSubscriptions :: (Member Query r, Member Exec r)
                     => UserId -> Eff r (S.Set ChanId)
getUserSubscriptions uid = do
    checkAccess uid forUserSelfOrAdmins
    Q.getUserSubscriptions uid

getUserContacts :: (Member Query r, Member Exec r)
                     => UserId -> Eff r (S.Set UserId)
getUserContacts uid = do
    checkAccess uid forUserSelfOrAdmins
    Q.getUserContacts uid

getUserNotifications :: (Member Query r, Member Exec r)
                     => UserId -> Eff r [Notification] 
getUserNotifications uid = do
    checkAccess uid forUserSelfOrAdmins
    Q.getUserNotifications uid

setUserLogin :: (Member Update r, Member Query r, Member Exec r)
             => UserId -> Login -> Eff r ()
setUserLogin uid l = do
    checkAccess uid forUserSelfOrAdmins
    l' <- getUserLogin uid
    if l == l'
        then return ()
        else do
            checkDuplicateLogin l
            U.setUserLogin uid l

setUserPassword :: (Member Update r, Member Query r, Member Exec r)
            => UserId -> Password -> Eff r ()
setUserPassword uid v = do
    checkAccess uid forUserSelfOrAdmins
    U.setUserPassword uid v


setUserName :: (Member Update r, Member Query r, Member Exec r)
            => UserId -> Name -> Eff r ()
setUserName uid v = do
    checkAccess uid forUserSelfOrAdmins
    U.setUserName uid v

setUserDesc :: (Member Update r, Member Query r, Member Exec r)
            => UserId -> Desc -> Eff r ()
setUserDesc uid v = do
    checkAccess uid forUserSelfOrAdmins
    U.setUserDesc uid v

setUserIcon :: (Member Update r, Member Query r, Member Exec r)
            => UserId -> Maybe Icon -> Eff r ()
setUserIcon uid v = do
    checkAccess uid forUserSelfOrAdmins
    U.setUserIcon uid v

addUserContact :: (Member Update r, Member Query r, Member Exec r)
               => UserId -> UserId -> Eff r ()
addUserContact uid other = do
    checkAccess uid forUserSelfOrAdmins
    U.addUserContact uid other

rmUserContact :: (Member Update r, Member Query r, Member Exec r)
              => UserId -> UserId -> Eff r ()
rmUserContact uid other = do
    checkAccess uid forUserSelfOrAdmins
    U.rmUserContact uid other

addUserNotification :: (Member Update r, Member Query r, Member Exec r)
                    => UserId -> Notification -> Eff r ()
addUserNotification uid notf = do
    checkAccess uid forUserContactsSelfOrAdmins
    U.addUserNotification uid notf

tryToAddUserNotification :: (Member Update r, Member Query r, Member Exec r)
                         => UserId -> Notification -> Eff r ()
tryToAddUserNotification uid notf = do
    tryAccess uid forUserContactsSelfOrAdmins $ do
        U.addUserNotification uid notf
    return ()

getUserIdByLogin :: (Member Query r, Member Exec r)
               => Text -> Eff r UserId
getUserIdByLogin l = do
    forceOperatorId
    res <- Q.getUserIdByLogin l
    case res of
        (Just user) -> return $ user
        otherwise -> E.throwME $ UnknownLogin l 

--------------------
-- creation of users
--------------------

createUser :: (Member Update r, Member Query r, Member Exec r)
           => Login -> Password -> Eff r UserId
createUser l pw = do
    checkAccess () forAdmins
    U.createUser l pw

-----------------------
-- creation of channels
-----------------------

createChannel :: (Member Update r, Member Query r, Member Exec r)
              => Name -> Eff r ChanId
createChannel n = do
    oid <- forceOperatorId
    U.createChan oid n

----------------------
-- posting of messages
----------------------

post :: (Member Update r, Member Query r, Member Exec r)
     => ChanId -> UserId -> UTCTime -> Text -> Maybe Image -> Eff r MsgId
post cid uid ts txt img = do
    checkAccess cid forProducersOrOwners
    U.post cid uid ts txt img

----------------------
-- reading of messages
----------------------

messages :: (Member Query r, Member Exec r)
         => ChanId -> Offset -> Amount -> Eff r [Message]
messages cid ofs am = do
    checkAccess cid forConsumersOrOwners
    Q.messages cid ofs am

messagesTill :: (Member Query r, Member Exec r) 
             => ChanId -> UTCTime -> Eff r [Message]
messagesTill cid ts = do
    checkAccess cid forConsumersOrOwners
    Q.messagesTill cid ts

----------
-- helpers
----------

checkDuplicateLogin l = do
    uid <- Q.getUserIdByLogin . loginToText $ l
    DuplicateLogin l `throwOn` (isJust uid) 
