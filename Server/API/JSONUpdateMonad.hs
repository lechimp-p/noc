{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module API.JSONUpdateMonad
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
import Web.Routes (RouteT, liftRouteT)
import qualified Web.Routes as WR
import Web.Routes.Happstack
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import API.APIMonad
import API.Errors
import API.JSONUtils
import ACID
import Model


newtype JSONUpdateMonadT acid url session m a = 
    JSONUpdateMonadT { runJSONUpdateMonadT :: JSONMonadT (UpdateMonadT acid (EitherT Error (APIMonadT url session m))) a}
    deriving (Monad, MonadIO, Functor, Applicative, MonadPlus)
                            
instance Monad m => MonadJSONError (UpdateMonadT acid (EitherT Error (APIMonadT url session m))) where
    throwJSONError = lift . throwJSONError

instance Monad m => MonadJSONError (JSONUpdateMonadT acid url session m) where
    throwJSONError = JSONUpdateMonadT . lift . throwJSONError

instance Monad m => MonadUpdateError (JSONUpdateMonadT acid url session m) where
    throwUpdateError = JSONUpdateMonadT . lift . lift . throwUpdateError 

instance Monad m => MonadJSON (JSONUpdateMonadT acid url session m) where
    readProp = JSONUpdateMonadT . readProp
    writeProp n = JSONUpdateMonadT . writeProp n
    writeListProp n = JSONUpdateMonadT . writeListProp n . fmap runJSONUpdateMonadT 
    writeObjectProp n = JSONUpdateMonadT . writeObjectProp n . runJSONUpdateMonadT 

instance Monad m => MonadUpdate (JSONUpdateMonadT NoC url session m) where
    doLoginU l = JSONUpdateMonadT . lift . doLoginU l
    getOperatorIdU = JSONUpdateMonadT . lift $ getOperatorIdU 
    addAdminU = JSONUpdateMonadT . lift . addAdminU
    rmAdminU = JSONUpdateMonadT . lift . rmAdminU
    getChanNameU = JSONUpdateMonadT . lift . getChanNameU
    getChanDescU = JSONUpdateMonadT . lift . getChanDescU
    setChanNameU c = JSONUpdateMonadT . lift . setChanNameU c
    setChanDescU c = JSONUpdateMonadT . lift . setChanDescU c
    addChanOwnerU c = JSONUpdateMonadT . lift . addChanOwnerU c
    addChanProducerU c = JSONUpdateMonadT . lift . addChanProducerU c
    addChanConsumerU c = JSONUpdateMonadT . lift . addChanConsumerU c
    rmChanOwnerU c = JSONUpdateMonadT . lift . rmChanOwnerU c
    rmChanProducerU c = JSONUpdateMonadT . lift . rmChanProducerU c
    rmChanConsumerU c = JSONUpdateMonadT . lift . rmChanConsumerU c
    subscribeToChanU u = JSONUpdateMonadT . lift . subscribeToChanU u
    unsubscribeFromChanU u = JSONUpdateMonadT . lift . unsubscribeFromChanU u
    getUserLoginU = JSONUpdateMonadT . lift . getUserLoginU
    getUserNameU = JSONUpdateMonadT . lift . getUserNameU
    getUserDescU = JSONUpdateMonadT . lift . getUserDescU 
    getUserIconU = JSONUpdateMonadT . lift . getUserIconU
    setUserLoginU u = JSONUpdateMonadT . lift . setUserLoginU u
    setUserPasswordU u = JSONUpdateMonadT . lift . setUserPasswordU u
    setUserNameU u = JSONUpdateMonadT . lift . setUserNameU u
    setUserDescU u = JSONUpdateMonadT . lift . setUserDescU u
    setUserIconU u = JSONUpdateMonadT . lift . setUserIconU u
    getUserOwnedChannelsU = JSONUpdateMonadT . lift . getUserOwnedChannelsU 
    getUserSubscriptionsU = JSONUpdateMonadT . lift . getUserSubscriptionsU
    getUserContactsU = JSONUpdateMonadT . lift . getUserContactsU
    getUserByLoginU = JSONUpdateMonadT . lift . getUserByLoginU
    createUserU l = JSONUpdateMonadT . lift . createUserU l
    createChannelU n = JSONUpdateMonadT . lift . createChannelU n
    postU c ts t = JSONUpdateMonadT . lift . postU c ts t
    messagesU c o = JSONUpdateMonadT . lift . messagesU c o
