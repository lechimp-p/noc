{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

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
    {--ChanQuery cid q 
        -> evalChanQuery noc q cid
    UserQuery uid q 
        -> evalUserQuery noc q uid
    --}
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
evalUpdate state q = undefined


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
evalExec state uid q = undefined

