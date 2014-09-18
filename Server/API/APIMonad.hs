{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module API.APIMonad
where

import Happstack.Server 
        ( ServerPartT, Response )
import Happstack.Server.Monads 
        ( FilterMonad, setFilter, getFilter
        , composeFilter, ServerMonad, askRq
        , localRq, mapServerPartT, UnWebT
        )
import Happstack.Server.RqData
        ( HasRqData, askRqEnv, localRqEnv
        , rqDataError
        )
import Happstack.Server.ClientSession
        ( ClientSessionT, MonadClientSession 
        , getSession, putSession, expireSession
        , ClientSession, mapClientSessionT
        ) 
import Happstack.Server.Internal.Monads
import Web.Routes (RouteT, liftRouteT)
import qualified Web.Routes as WR
import Web.Routes.Happstack
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Lens (view)

import API.Config

type InnerAPIMonadT session m = (ClientSessionT session (ServerPartT m))
newtype APIMonadT url session m a = APIMonadT { unAPIMonadT :: ReaderT Config (RouteT url (InnerAPIMonadT session m)) a }
                                    deriving (Monad, MonadPlus, MonadIO, Applicative, Functor)

instance Monad m => WithConfig (APIMonadT url session m) where
    config = APIMonadT . asks . view 

instance (Monad m)
      => FilterMonad Response (APIMonadT url session m) 
    where
    setFilter = APIMonadT . setFilter 
    composeFilter = APIMonadT . composeFilter 
    getFilter = APIMonadT . getFilter . unAPIMonadT

--instance MonadTrans (APIMonadT url session) where
--    lift = APIMonadT

instance ( Functor m
         , MonadIO m
         , ClientSession session
         ) 
      => MonadClientSession session (APIMonadT url session m) 
    where
    getSession = APIMonadT . lift . liftRouteT $ getSession
    putSession = APIMonadT . lift . liftRouteT . putSession
    expireSession = APIMonadT . lift . liftRouteT $ expireSession

instance Monad m => ServerMonad (APIMonadT url session m) where
    askRq = APIMonadT askRq
    localRq f = APIMonadT . localRq f . unAPIMonadT

instance Monad m => WebMonad Response (APIMonadT url session m) where
    finishWith = APIMonadT . finishWith

instance MonadIO m => HasRqData (APIMonadT url session m) where
    askRqEnv = APIMonadT $ askRqEnv
    rqDataError = APIMonadT . rqDataError
    localRqEnv f = APIMonadT . localRqEnv f . unAPIMonadT

nestURL :: Monad m => (url1 -> url2) -> APIMonadT url1 session m a -> APIMonadT url2 session m a
nestURL f m2 = do
    cfg <- config id
    APIMonadT . lift . WR.nestURL f . flip runReaderT cfg . unAPIMonadT $ m2

{--
mapAPIMonadT :: (m a -> n a) -> APIMonadT url session m a -> APIMonadT url session n a
mapAPIMonadT f = APIMonadT . WR.mapRouteT (mapClientSessionT (mapServerPartT unWebFun)) . unAPIMonadT 
    where
    unWebFun :: UnWebT m a -> UnWebT n a
    unWebFun = error "NYI!"
--}

