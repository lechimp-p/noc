{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Acid
    ( runQuery
    , runQueryAndUpdate
    , runSimple
    )
where

import Model.BaseTypes
import Model.Errors
import Model.Query
import Model.Update
import Model.Exec

import Model.Simple.NoC (NoC)
import qualified Model.Acid.Query as AQ
import qualified Model.Acid.Update as AU

import Control.Lens 
import Control.Eff
import Control.Eff.Lift
import Control.Monad.IO.Class
import qualified Data.Acid as A
import qualified Data.Acid.Advanced as AA
import qualified Data.Set as S 
import qualified Data.IxSet as IX
import Data.Typeable.Internal (Typeable1)
import Data.Time.Clock (UTCTime)


runQuery :: (Typeable1 m, Functor m, MonadIO m, SetMember Lift (Lift m) r)
         => A.AcidState NoC -> Eff (Query :> Exec :> r) a -> Eff (Exec :> r) a
runQuery state action = go (admin action)
    where
    go (Val v) = return v
    go (E request) = handleRelay request go
        $ \ req -> let act = evalQuery state req in do
                res <- lift act 
                either throwME go res

evalQuery :: (Functor m, MonadIO m, Member Query r)
          => A.AcidState NoC -> Query (VE r w) -> m (Either Error (VE r w))
evalQuery state q = case q of
    IsAdmin uid next -> ffq next (AQ.IsAdmin uid)
    CountAdmins next -> ffq next AQ.CountAdmins
    GetUserIdByLogin l next -> ffq next (AQ.GetUserIdByLogin l)
    ChanQuery cid q 
        -> evalChanQuery state q cid
    UserQuery uid q 
        -> evalUserQuery state q uid
    where
    ffq n = fmap (fmap n) . AA.query' state

evalChanQuery :: (Functor m, MonadIO m, Member Query r)
              => A.AcidState NoC -> ChanQueryType (VE r w) -> ChanId -> m (Either Error (VE r w))
evalChanQuery state q cid = case q of
    GetChanName next -> ffq next (AQ.GetChanName cid)
    GetChanDesc next -> ffq next (AQ.GetChanDesc cid)
    GetChanType next -> ffq next (AQ.GetChanType cid)
    GetChanImage next -> ffq next (AQ.GetChanImage cid)
    IsChanOwner uid next -> ffq next (AQ.IsChanOwner cid uid)
    IsChanProducer uid next -> ffq next (AQ.IsChanProducer cid uid)
    IsChanConsumer uid next -> ffq next (AQ.IsChanConsumer cid uid)
    AmountOfSubscribedUsers next -> ffq next (AQ.AmountOfSubscribedUsers cid)
    LastPostTimestamp next ->  ffq next (AQ.LastPostTimestamp cid)
    Messages ofs am next -> ffq next (AQ.Messages cid ofs am) 
    MessagesTill ts next -> ffq next (AQ.MessagesTill cid ts)
    where
    ffq n = fmap (fmap n) . AA.query' state

evalUserQuery :: (Functor m, MonadIO m, Member Query r)
              => A.AcidState NoC -> UserQueryType (VE r w) -> UserId -> m (Either Error (VE r w))
evalUserQuery state q uid = case q of
    GetUserLogin next -> ffq next (AQ.GetUserLogin uid)
    GetUserName next -> ffq next (AQ.GetUserName uid)
    GetUserDesc next -> ffq next (AQ.GetUserDesc uid)
    GetUserIcon next -> ffq next (AQ.GetUserIcon uid)
    GetUserNotifications next -> ffq next (AQ.GetUserNotifications uid)
    GetUserContacts next -> ffq next (AQ.GetUserContacts uid)
    GetUserSubscriptions next -> ffq next (AQ.GetUserSubscriptions uid)
    where
    ffq n = fmap (fmap n) . AA.query' state


runQueryAndUpdate :: (Typeable1 m, Functor m, MonadIO m, SetMember Lift (Lift m) r)
                  => A.AcidState NoC -> Eff (Query :> Update :> Exec :> r) a -> Eff (Exec :> r) a
runQueryAndUpdate state action = go (admin action)
    where
    go (Val v) = return v
    go (E request) = checkQuery request

    checkQuery r = flip (either checkUpdate) (decomp r)
                        $ \ req -> let act = evalQuery state req in do
                                res <- lift act 
                                either throwME go res
    checkUpdate r = flip (either passOn) (decomp r)
                        $ \ req -> let act = evalUpdate state req in do
                                res <- lift act 
                                either throwME go res
    passOn r = send (flip fmap r) >>= go 

evalUpdate :: (Functor m, MonadIO m, Member Query r)
          => A.AcidState NoC -> Update (VE r w) -> m (Either Error (VE r w))
evalUpdate state q = case q of
    CreateChan uid name next -> ffu next (AU.CreateChan uid name)
    CreateUser l pw next -> ffu next (AU.CreateUser l pw)
    AddAdmin uid next -> ffu next (AU.AddAdmin uid)
    RmAdmin uid next -> ffu next (AU.RmAdmin uid)
    ChanUpdate cid q -> evalChanUpdate state cid q
    UserUpdate uid q -> evalUserUpdate state uid q 
    where
    ffu n = fmap (fmap n) . AA.update' state

evalChanUpdate :: (Functor m, MonadIO m, Member Query r)
               => A.AcidState NoC -> ChanId -> ChanUpdateType (VE r w) -> m (Either Error (VE r w))
evalChanUpdate state cid q = case q of
    SetChanName n next -> ffu next (AU.SetChanName cid n)
    SetChanDesc d next -> ffu next (AU.SetChanDesc cid d)
    SetChanType t next -> ffu next (AU.SetChanType cid t)
    SetChanImage i next -> ffu next (AU.SetChanImage cid i)
    AddChanOwner uid next -> ffu next (AU.AddChanOwner cid uid)
    RmChanOwner uid next -> ffu next (AU.RmChanOwner cid uid)
    AddChanProducer uid next -> ffu next (AU.AddChanProducer cid uid)
    RmChanProducer uid next -> ffu next (AU.RmChanProducer cid uid)
    AddChanConsumer uid next -> ffu next (AU.AddChanConsumer cid uid)
    RmChanConsumer uid next -> ffu next (AU.RmChanConsumer cid uid)
    Post uid ts txt img next -> ffu next (AU.Post cid uid ts txt img)
    where
    ffu n = fmap (fmap n) . AA.update' state
 
evalUserUpdate :: (Functor m, MonadIO m, Member Query r)
               => A.AcidState NoC -> UserId -> UserUpdateType (VE r w) -> m (Either Error (VE r w))
evalUserUpdate state uid q = case q of
    SetUserLogin l next -> ffu next (AU.SetUserLogin uid l)
    SetUserPassword pw next -> ffu next (AU.SetUserPassword uid pw)
    SetUserName n next -> ffu next (AU.SetUserName uid n)
    SetUserDesc d next -> ffu next (AU.SetUserDesc uid d)
    SetUserIcon i next -> ffu next (AU.SetUserIcon uid i)
    AddUserNotification n next -> ffu next (AU.AddUserNotification uid n)
    AddUserContact uid' next -> ffu next (AU.AddUserContact uid uid')
    RmUserContact uid' next -> ffu next (AU.RmUserContact uid uid')
    AddUserSubscription cid next -> ffu next (AU.AddUserSubscription uid cid)
    RmUserSubscription cid next -> ffu next (AU.RmUserSubscription uid cid)
    where
    ffu n = fmap (fmap n) . AA.update' state

runSimple :: (Typeable1 m, Functor m, MonadIO m, SetMember Lift (Lift m) r)
          => A.AcidState NoC -> Maybe UserId -> Eff (Query :> Update :> Exec :> r) a -> Eff r (Either Error a)
runSimple state uid action = go uid (admin action)
    where
    go _ (Val v) = return . Right $ v
    go uid (E request) = checkQuery uid request 
 
    checkQuery u r = flip (either (checkUpdate uid)) (decomp r)
                        $ \ req -> let act = evalQuery state req in do
                                res <- lift act 
                                either (return . Left) (go u) res
    checkUpdate u r = flip (either (checkExec uid)) (decomp r)
                        $ \ req -> let act = evalUpdate state req in do
                                res <- lift act 
                                either (return . Left) (go u) res
    checkExec u r = flip (either (passOn u)) (decomp r)
                        $ \ req -> let act = evalExec state u req in do
                                res <- lift act 
                                either (return . Left) (uncurry go) res
    passOn u r = send (flip fmap r) >>= go u 

evalExec :: (Functor m, MonadIO m, Member Query r)
          => A.AcidState NoC -> Maybe UserId 
          -> Exec (VE r w) -> m (Either Error (Maybe UserId , (VE r w)))
evalExec state uid q = case q of 
    GetOperatorId next -> return $ Right (uid, next uid) 
    ThrowME err next -> return $ Left $ err 
    DoLogout next -> return $ Right (uid, next ())
    DoLogin l pw next -> do
            res <- AA.query' state (AQ.DoLogin l pw)
            case res of
                Nothing -> return . Left $ CantLogin l 
                Just id -> return $ Right (Just id, next id)

