{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module API.Monad
where

import Happstack.Server 
        ( ServerPartT, Response )
import Happstack.Server.Monads 
        ( FilterMonad, setFilter, getFilter
        , composeFilter, ServerMonad, askRq
        , localRq
        )
import Happstack.Server.ClientSession
        ( ClientSessionT, MonadClientSession 
        , getSession, putSession, expireSession
        , ClientSession
        ) 
import Web.Routes (RouteT, liftRouteT)
import qualified Web.Routes as WR
import Web.Routes.Happstack
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

type InnerAPIMonad session = (ClientSessionT session (ServerPartT IO))
newtype APIMonad url session a = APIMonad { unAPIMonad :: RouteT url (InnerAPIMonad session) a }
                                 deriving (Monad, MonadPlus, MonadIO, Applicative, Functor)

instance FilterMonad Response (APIMonad url session) where
    setFilter = APIMonad . setFilter 
    composeFilter = APIMonad . composeFilter
    getFilter = APIMonad . getFilter . unAPIMonad

instance (ClientSession session) 
      => MonadClientSession session (APIMonad url session) 
    where
    getSession = APIMonad . liftRouteT $ getSession
    putSession = APIMonad . liftRouteT . putSession
    expireSession = APIMonad . liftRouteT $ expireSession

instance ServerMonad (APIMonad url session) where
    askRq = APIMonad askRq
    localRq f = APIMonad . localRq f . unAPIMonad

nestURL :: (url1 -> url2) -> APIMonad url1 session a -> APIMonad url2 session a
nestURL f = APIMonad . WR.nestURL f . unAPIMonad
