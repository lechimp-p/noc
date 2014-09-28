--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Model.Simple
    ( runQuery
    , runQueryAndUpdate
    , runSimple
    --, run
    )
where

import Model.BaseTypes
import Model.Errors
import Model.Query
import Model.Update
import Model.Exec

import qualified Model.Simple.NoC as N
import Model.Simple.NoC
import qualified Model.Simple.User as U
import Model.Simple.User
import qualified Model.Simple.Channel as C
import Model.Simple.Channel
import qualified Model.Simple.Message as M
import Model.Simple.Message
import Model.Simple.Operations

import Control.Eff
import qualified Data.IxSet as IX


runQuery :: NoC -> Eff (Query :> Exec :> r) a -> Eff (Exec :> r) a
runQuery noc action = go noc (admin action)
    where
    go _ (Val v) = return v
    go noc (E request) = handleRelay request (go noc) 
        $ \ req -> case evalQuery noc req of 
            Right next -> go noc next 
            Left err -> throwME err

evalQuery :: (Member Query r)
          => NoC -> Query (VE r w) -> Either Error (VE r w) 
evalQuery noc q = case q of
    IsAdmin uid next -> Right . next . isAdminR noc $ uid
    CountAdmins next -> Right . next . countAdminsR $ noc
    GetUserIdByLogin l next -> Right . next . getUserIdByLoginR noc $ l
    ChanQuery cid q -> evalChanQuery noc q cid
    UserQuery uid q -> evalUserQuery noc q uid

evalChanQuery :: (Member Query r)
              => NoC -> ChanQueryType (VE r a) -> ChanId -> Either Error (VE r a) 
evalChanQuery noc q cid = case q of
    GetChanName next -> fmap next . getChanNameR noc $ cid
    GetChanDesc next -> fmap next . getChanDescR noc $ cid
    GetChanType next -> fmap next . getChanTypeR noc $ cid
    GetChanImage next -> fmap next . getChanImageR noc $ cid
    IsChanOwner uid next -> fmap next . isChanOwnerR noc cid $ uid
    IsChanProducer uid next -> fmap next . isChanProducerR noc cid $ uid 
    IsChanConsumer uid next -> fmap next . isChanConsumerR noc cid $ uid
    AmountOfSubscribedUsers next -> fmap next . amountOfSubscribedUsersR noc $ cid
    LastPostTimestamp next -> fmap next . lastPostTimestampR noc $ cid 
    Messages ofs am next -> fmap next . messagesR noc cid ofs $ am
    MessagesTill ts next -> fmap next . messagesTillR noc cid $ ts

evalUserQuery :: (Member Query r)
              => NoC -> UserQueryType (VE r a) -> UserId -> Either Error (VE r a) 
evalUserQuery noc q uid = case q of
    GetUserLogin next -> fmap next . getUserLoginR noc $ uid
    GetUserName next -> fmap next . getUserNameR noc $ uid
    GetUserDesc next -> fmap next . getUserDescR noc $ uid
    GetUserIcon next -> fmap next . getUserIconR noc $ uid
    GetUserNotifications next -> fmap next . getUserNotificationsR noc $ uid
    GetUserContacts next -> fmap next . getUserContactsR noc $ uid
    GetUserSubscriptions next -> fmap next . getUserSubscriptionsR noc $ uid


runQueryAndUpdate :: NoC -> Eff (Query :> Update :> Exec :> r) a -> Eff (Exec :> r) (NoC, a)
runQueryAndUpdate noc action = go noc (admin action)
    where
    go n (Val v) = return (n, v) 
    go n (E request) = checkQuery n request

    checkQuery n r = flip (either (checkUpdate n)) (decomp r)
                        $ \ req -> case evalQuery n req of 
                            Right next -> go n next
                            Left err -> throwME err
    checkUpdate n r = flip (either (passOn n)) (decomp r) 
                        $ \ req -> case evalUpdate n req of
                            Right (noc', next) -> go noc' next
                            Left err -> throwME err
    passOn n r = send (flip fmap r) >>= go n 

evalUpdate :: (Member Update r)
           => NoC -> Update (VE r w) -> Either Error (NoC, (VE r w))
evalUpdate noc q = case q of
    CreateChan uid name next -> Right . fmap next . createChanR noc uid $ name 
    CreateUser l pw next -> Right . fmap next . createUserR noc l $ pw
    AddAdmin uid next -> Right . fmap next . addAdminR noc $ uid
    RmAdmin uid next -> Right . fmap next . rmAdminR noc $ uid
    ChanUpdate cid q -> evalChanUpdate noc cid q 
    UserUpdate uid q -> evalUserUpdate noc uid q

evalChanUpdate noc cid q = case q of
    SetChanName n next -> fmap (fmap next) . setChanNameR noc cid $ n
    SetChanDesc d next -> fmap (fmap next) . setChanDescR noc cid $ d
    SetChanType t next -> fmap (fmap next) . setChanTypeR noc cid $ t
    AddChanOwner uid next -> fmap (fmap next) . addChanOwnerR noc cid $ uid
    RmChanOwner uid next -> fmap (fmap next) . rmChanOwnerR noc cid $ uid
    AddChanProducer uid next -> fmap (fmap next) . addChanOwnerR noc cid $ uid
    RmChanProducer uid next -> fmap (fmap next) . rmChanOwnerR noc cid $ uid
    AddChanConsumer uid next -> fmap (fmap next) . addChanConsumerR noc cid $ uid
    RmChanConsumer uid next -> fmap (fmap next) . rmChanConsumerR noc cid $ uid
    Post uid ts txt img next -> fmap (fmap next) . postR noc cid uid ts txt $ img
      
evalUserUpdate noc uid q = case q of
    SetUserLogin l next -> fmap (fmap next) . setUserLoginR noc uid $ l 
    SetUserPassword pw next -> fmap (fmap next) . setUserPasswordR noc uid $ pw
    SetUserName n next -> fmap (fmap next) . setUserNameR noc uid $ n
    SetUserDesc d next -> fmap (fmap next) . setUserDescR noc uid $ d
    SetUserIcon i next -> fmap (fmap next) . setUserIconR noc uid $ i
    AddUserNotification n next -> fmap (fmap next) . addUserNotificationR noc uid $ n
    AddUserContact uid' next -> fmap (fmap next) . addUserContactR noc uid $ uid'
    RmUserContact uid' next -> fmap (fmap next) . rmUserContactR noc uid $ uid'
    AddUserSubscription cid next -> fmap (fmap next) . addUserSubscriptionR noc uid $ cid
    RmUserSubscription cid next -> fmap (fmap next) . rmUserSubscriptionR noc uid $ cid

runSimple :: NoC -> Maybe UserId -> Eff (Query :> Update :> Exec :> r) a 
          -> Eff r (Either Error (NoC, a))
runSimple noc uid action = go noc uid (admin action)
    where
    go n _ (Val v) = return . Right $ (n, v)
    go n u (E request) = checkQuery n u request

    checkQuery n u r = flip (either (checkUpdate n u)) (decomp r)
                        $ \ req -> case evalQuery n req of 
                            Right next -> go n u next
                            Left err -> return . Left $ err
    checkUpdate n u r = flip (either (checkExec n u)) (decomp r) 
                        $ \ req -> case evalUpdate n req of
                            Right (noc', next) -> go noc' u next
                            Left err -> return . Left $ err
    checkExec n u r = flip (either (passOn n u)) (decomp r)
                        $ \ req -> case evalExec n u req of
                            Right (next, noc', uid') -> go noc' uid' next 
                            Left err -> return . Left $ err
    passOn n u r = send (flip fmap r) >>= go n u

evalExec noc uid q = case q of
    GetOperatorId next
        -> Right (next uid, noc, uid) 
    ThrowME err next
        -> Left $ err 
    DoLogout next
        -> Right (next (), noc, uid)
    DoLogin l pw next
        -> case _login l pw of
            Nothing -> Left $ CantLogin l 
            Just id -> Right (next id, noc, Just id)
    where 
    _login l pw = do
        user <- IX.getOne (_users noc IX.@= l)
        if checkPassword pw (U._password user)
            then return (U._id user)
            else fail "password mismatch"
