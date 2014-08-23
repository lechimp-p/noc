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

type InnerAPIMonadT session m = (ClientSessionT session (ServerPartT m))
newtype APIMonadT url session m a = APIMonadT { unAPIMonadT :: RouteT url (InnerAPIMonadT session m) a }
                                    deriving (Monad, MonadPlus, MonadIO, Applicative, Functor)

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
    getSession = APIMonadT . liftRouteT $ getSession
    putSession = APIMonadT . liftRouteT . putSession
    expireSession = APIMonadT . liftRouteT $ expireSession

instance Monad m => ServerMonad (APIMonadT url session m) where
    askRq = APIMonadT askRq
    localRq f = APIMonadT . localRq f . unAPIMonadT

instance Monad m => WebMonad Response (APIMonadT url session m) where
    finishWith = APIMonadT . finishWith


nestURL :: (url1 -> url2) -> APIMonadT url1 session a m -> APIMonadT url2 session a m
nestURL f = APIMonadT . WR.nestURL f . unAPIMonadT

{--
mapAPIMonadT :: (m a -> n a) -> APIMonadT url session m a -> APIMonadT url session n a
mapAPIMonadT f = APIMonadT . WR.mapRouteT (mapClientSessionT (mapServerPartT unWebFun)) . unAPIMonadT 
    where
    unWebFun :: UnWebT m a -> UnWebT n a
    unWebFun = error "NYI!"
--}
