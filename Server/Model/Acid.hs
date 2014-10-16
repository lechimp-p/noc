{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.Acid
    ( runQuery
    , runQueryAndUpdate
    , runAcid
    , NoC
    , mkNoC
    , A.AcidState
    )
where

import Model.BaseTypes
import Model.Errors
import Model.Query
import Model.Update
import Model.Exec

import Model.Simple.NoC (NoC, mkNoC)
import Model.Acid.Acidic

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
    IsAdmin uid next -> ffq next (QIsAdmin uid)
    CountAdmins next -> ffq next QCountAdmins
    GetUserIdByLogin l next -> ffq next (QGetUserIdByLogin l)
    SearchUserByLogin l next -> ffq next (QSearchUserByLogin l)
    SearchChanByName n next -> ffq next (QSearchChanByName n)
    ChanQuery cid q 
        -> evalChanQuery state q cid
    UserQuery uid q 
        -> evalUserQuery state q uid
    where
    ffq n = fmap (fmap n) . AA.query' state

evalChanQuery :: (Functor m, MonadIO m, Member Query r)
              => A.AcidState NoC -> ChanQueryType (VE r w) -> ChanId -> m (Either Error (VE r w))
evalChanQuery state q cid = case q of
    GetChanName next -> ffq next (QGetChanName cid)
    GetChanDesc next -> ffq next (QGetChanDesc cid)
    GetChanType next -> ffq next (QGetChanType cid)
    GetChanImage next -> ffq next (QGetChanImage cid)
    IsChanOwner uid next -> ffq next (QIsChanOwner cid uid)
    IsChanProducer uid next -> ffq next (QIsChanProducer cid uid)
    IsChanConsumer uid next -> ffq next (QIsChanConsumer cid uid)
    AmountOfSubscribedUsers next -> ffq next (QAmountOfSubscribedUsers cid)
    GetChanSubscribers next -> ffq next (QGetChanSubscribers cid)
    LastPostTimestamp next ->  ffq next (QLastPostTimestamp cid)
    Messages ofs am next -> ffq next (QMessages cid ofs am) 
    MessagesTill ts next -> ffq next (QMessagesTill cid ts)
    where
    ffq n = fmap (fmap n) . AA.query' state

evalUserQuery :: (Functor m, MonadIO m, Member Query r)
              => A.AcidState NoC -> UserQueryType (VE r w) -> UserId -> m (Either Error (VE r w))
evalUserQuery state q uid = case q of
    GetUserLogin next -> ffq next (QGetUserLogin uid)
    GetUserName next -> ffq next (QGetUserName uid)
    GetUserDesc next -> ffq next (QGetUserDesc uid)
    GetUserIcon next -> ffq next (QGetUserIcon uid)
    GetUserEmail next -> ffq next (QGetUserEmail uid)
    GetUserNotifications next -> ffq next (QGetUserNotifications uid)
    GetUserContacts next -> ffq next (QGetUserContacts uid)
    GetUserSubscriptions next -> ffq next (QGetUserSubscriptions uid)
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
    CreateChan uid name next -> ffu next (UCreateChan uid name)
    CreateUser l pw next -> ffu next (UCreateUser l pw)
    AddAdmin uid next -> ffu next (UAddAdmin uid)
    RmAdmin uid next -> ffu next (URmAdmin uid)
    ChanUpdate cid q -> evalChanUpdate state cid q
    UserUpdate uid q -> evalUserUpdate state uid q 
    where
    ffu n = fmap (fmap n) . AA.update' state

evalChanUpdate :: (Functor m, MonadIO m, Member Query r)
               => A.AcidState NoC -> ChanId -> ChanUpdateType (VE r w) -> m (Either Error (VE r w))
evalChanUpdate state cid q = case q of
    SetChanName n next -> ffu next (USetChanName cid n)
    SetChanDesc d next -> ffu next (USetChanDesc cid d)
    SetChanType t next -> ffu next (USetChanType cid t)
    SetChanImage i next -> ffu next (USetChanImage cid i)
    AddChanOwner uid next -> ffu next (UAddChanOwner cid uid)
    RmChanOwner uid next -> ffu next (URmChanOwner cid uid)
    AddChanProducer uid next -> ffu next (UAddChanProducer cid uid)
    RmChanProducer uid next -> ffu next (URmChanProducer cid uid)
    AddChanConsumer uid next -> ffu next (UAddChanConsumer cid uid)
    RmChanConsumer uid next -> ffu next (URmChanConsumer cid uid)
    Post uid ts txt img next -> ffu next (UPost cid uid ts txt img)
    where
    ffu n = fmap (fmap n) . AA.update' state
 
evalUserUpdate :: (Functor m, MonadIO m, Member Query r)
               => A.AcidState NoC -> UserId -> UserUpdateType (VE r w) -> m (Either Error (VE r w))
evalUserUpdate state uid q = case q of
    SetUserLogin l next -> ffu next (USetUserLogin uid l)
    SetUserPassword pw next -> ffu next (USetUserPassword uid pw)
    SetUserName n next -> ffu next (USetUserName uid n)
    SetUserDesc d next -> ffu next (USetUserDesc uid d)
    SetUserIcon i next -> ffu next (USetUserIcon uid i)
    SetUserEmail e next -> ffu next (USetUserEmail uid e)
    AddUserNotification n next -> ffu next (UAddUserNotification uid n)
    AddUserContact uid' next -> ffu next (UAddUserContact uid uid')
    RmUserContact uid' next -> ffu next (URmUserContact uid uid')
    AddUserSubscription cid next -> ffu next (UAddUserSubscription uid cid)
    RmUserSubscription cid next -> ffu next (URmUserSubscription uid cid)
    where
    ffu n = fmap (fmap n) . AA.update' state

runAcid :: (Typeable1 m, Functor m, MonadIO m, SetMember Lift (Lift m) r)
        => A.AcidState NoC -> Maybe UserId -> Eff (Query :> Update :> Exec :> r) a -> Eff r (Either Error a)
runAcid state uid action = go uid (admin action)
    where
    go _ (Val v) = return . Right $ v
    go u (E request) = checkQuery u request 
 
    checkQuery u r = flip (either (checkUpdate u)) (decomp r)
                        $ \ req -> let act = evalQuery state req in do
                                res <- lift act 
                                either (return . Left) (go u) res
    checkUpdate u r = flip (either (checkExec u)) (decomp r)
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
            res <- AA.query' state (QDoLogin l pw)
            case res of
                Nothing -> return . Left $ CantLogin l 
                Just i  -> return $ Right (res, next i)

