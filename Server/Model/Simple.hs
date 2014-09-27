--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Lens 
import Control.Eff
import qualified Data.Set as S 
import qualified Data.IxSet as IX
import Data.Time.Clock (UTCTime)


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
    IsAdmin uid next 
        -> Right . next . S.member uid . _admins $ noc
    CountAdmins next 
        -> Right . next . S.size . _admins $ noc
    GetUserIdByLogin l next 
        -> Right . next . fmap U._id . IX.getOne $ _users noc IX.@= (Login l) 
    ChanQuery cid q 
        -> evalChanQuery noc q cid
    UserQuery uid q 
        -> evalUserQuery noc q uid

evalChanQuery :: (Member Query r)
              => NoC -> ChanQueryType (VE r a) -> ChanId -> Either Error (VE r a) 
evalChanQuery noc q cid = case q of
    GetChanName next 
        -> fmap next . queryChan C._name noc $ cid
    GetChanDesc next 
        -> fmap next . queryChan C._desc noc $ cid
    GetChanType next 
        -> fmap next . queryChan C._type' noc $ cid
    GetChanImage next 
        -> fmap next . queryChan C._image noc $ cid
    IsChanOwner uid next 
        -> fmap next . queryChan (S.member uid . C._owners) noc $ cid
    IsChanProducer uid next 
        -> fmap next . queryChan (S.member uid . C._producers) noc $ cid 
    IsChanConsumer uid next 
        -> fmap next . queryChan (S.member uid . C._consumers) noc $ cid
    AmountOfSubscribedUsers next 
        -> fmap next . queryChan (S.size . C._subscribers) noc $ cid
    LastPostTimestamp next 
        -> fmap next . queryChan (lastPostTimestamp' cid) noc $ cid 
    Messages ofs am next 
        -> fmap next . queryChan (messages' ofs am cid) noc $ cid
    MessagesTill ts next 
        -> fmap next . queryChan (messagesTill' ts cid) noc $ cid
    where
    lastPostTimestamp' cid _ = do
        msg <- head' . IX.toDescList (IX.Proxy :: IX.Proxy UTCTime) $ N._messages noc IX.@= cid
        return $ _timestamp msg
    messages' ofs am cid _ = 
        take am . drop ofs 
        . IX.toDescList (IX.Proxy :: IX.Proxy UTCTime) $ N._messages noc IX.@= cid
    messagesTill' ts cid _ = 
        takeWhile ((<=) ts . _timestamp) 
        . IX.toDescList (IX.Proxy :: IX.Proxy UTCTime) $ N._messages noc IX.@= cid
        
queryChan :: (Channel -> b)
          -> NoC 
          -> ChanId
          -> Either Error b 
queryChan fun noc cid = 
    let chan = IX.getOne (_channels noc IX.@= cid)
    in case chan of
        Nothing -> Left $ UnknownChannel cid
        Just c -> Right $ fun c

evalUserQuery :: (Member Query r)
              => NoC -> UserQueryType (VE r a) -> UserId -> Either Error (VE r a) 
evalUserQuery noc q = case q of
    GetUserLogin next
        -> fmap next . queryUser U._login noc
    GetUserName next
        -> fmap next . queryUser U._name noc
    GetUserDesc next 
        -> fmap next . queryUser U._desc noc
    GetUserIcon next
        -> fmap next . queryUser U._icon noc
    GetUserNotifications next
        -> fmap next . queryUser U._notifications noc
    GetUserContacts next
        -> fmap next . queryUser U._contacts noc
    GetUserSubscriptions next
        -> fmap next . queryUser U._subscriptions noc

queryUser :: (User -> b)
          -> NoC 
          -> UserId
          -> Either Error b 
queryUser fun noc uid = 
    let user = IX.getOne (_users noc IX.@= uid)
    in case user of
        Nothing -> Left $ UnknownUser uid
        Just u -> Right $ fun u


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
                            Right (next, noc') -> go noc' next
                            Left err -> throwME err
    passOn n r = send (flip fmap r) >>= go n 

evalUpdate :: (Member Update r)
           => NoC -> Update (VE r w) => Either Error ((VE r w), NoC)
evalUpdate noc q = case q of
    CreateChan uid name next
        -> Right (next cid, noc')
            where
            noc' = over channels (IX.insert chan) (set nextChanId ncid noc)
            chan = Channel cid name (mkDesc "") None Nothing (S.insert uid S.empty) S.empty S.empty S.empty S.empty  
            cid = _nextChanId noc
            ncid = ChanId(ciToInt cid + 1) 
    CreateUser l pw next
        -> Right (next uid, noc')
            where
            noc' = over users (IX.insert user) (set nextUserId nuid noc)
            user = User uid l pw (mkName "") (mkDesc "") Nothing S.empty S.empty S.empty []
            uid  = _nextUserId noc
            nuid = UserId (uiToInt uid + 1)
    AddAdmin uid next 
        -> Right (next (), over admins (S.insert uid) noc) 
    RmAdmin uid next 
        -> Right (next (), over admins (S.delete uid) noc) 
    ChanUpdate cid q 
        -> evalChanUpdate noc cid q 
    UserUpdate uid q
        -> evalUserUpdate noc uid q

evalChanUpdate noc cid q = case q of
    SetChanName n next
        -> updateChan (set C.name n) (next ()) noc cid
    SetChanDesc d next
        -> updateChan (set C.desc d) (next ()) noc cid
    SetChanType t next
        -> updateChan (set C.type' t) (next ()) noc cid
    AddChanOwner uid next
        -> updateChan (over C.owners (S.insert uid)) (next ()) noc cid
    RmChanOwner uid next
        -> updateChan (over C.owners (S.delete uid)) (next ()) noc cid
    AddChanProducer uid next
        -> updateChan (over C.producers (S.insert uid)) (next ()) noc cid
    RmChanProducer uid next
        -> updateChan (over C.producers (S.delete uid)) (next ()) noc cid
    AddChanConsumer uid next
        -> updateChan (over C.consumers (S.insert uid)) (next ()) noc cid
    RmChanConsumer uid next
        -> updateChan (over C.consumers (S.delete uid)) (next ()) noc cid
    Post uid ts txt img next
        -> updateChan (over C.messages (S.insert mid)) (next mid) noc' cid
            where
            noc' = over N.messages (IX.insert msg) (set nextMsgId nmid noc)
            msg = Message mid cid img txt uid ts
            mid = N._nextMsgId noc
            nmid = MsgId (miToInt mid + 1)
     
updateChan :: (Channel -> Channel)
           -> a
           -> NoC
           -> ChanId
           -> Either Error (a, NoC)
updateChan mod v noc cid = 
    let chan = IX.getOne (_channels noc IX.@= cid)
    in case chan of
        Nothing -> Left $ UnknownChannel cid
        Just c -> Right (v, over channels (IX.updateIx cid (mod c)) noc)
       
evalUserUpdate noc uid q = case q of
    SetUserLogin l next
        -> updateUser (set login l) (next ()) noc uid
    SetUserPassword pw next
        -> updateUser (set password pw) (next ()) noc uid
    SetUserName n next
        -> updateUser (set U.name n) (next ()) noc uid
    SetUserDesc d next
        -> updateUser (set U.desc d) (next ()) noc uid
    SetUserIcon i next
        -> updateUser (set icon i) (next ()) noc uid
    AddUserNotification n next
        -> updateUser (over notifications ((:) n)) (next ()) noc uid
    AddUserContact uid next
        -> updateUser (over contacts (S.insert uid)) (next ()) noc uid
    RmUserContact uid next
        -> updateUser (over contacts (S.delete uid)) (next ()) noc uid
    AddUserSubscription cid next
        -> updateUser (over subscriptions (S.insert cid)) (next ()) noc uid
    RmUserSubscription cid next
        -> updateUser (over subscriptions (S.delete cid)) (next ()) noc uid

updateUser :: (User -> User)
           -> a
           -> NoC
           -> UserId
           -> Either Error (a, NoC)
updateUser mod v noc uid =
    let user = IX.getOne (_users noc IX.@= uid)
    in case user of
        Nothing -> Left $ UnknownUser uid
        Just u -> Right (v, over users (IX.updateIx uid (mod u)) noc)


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
                            Right (next, noc') -> go noc' u next
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
    
head' []     = Nothing
head' (x:_)  = Just x
