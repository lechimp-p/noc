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
import Control.Monad.Trans.JSON

import API.APIMonad
import API.Errors
import API.ImageUtils
import ACID
import Model


newtype JSONUpdateMonadT acid url session m a = 
    JSONUpdateMonadT { runJSONUpdateMonadT :: JSONMonadT (UpdateMonadT acid (EitherT Error (APIMonadT url session m))) a}
    deriving (Monad, MonadIO, MonadPlus)
 
instance (Functor m, Monad m) => Functor (JSONUpdateMonadT acid url session m) where
    fmap f = JSONUpdateMonadT . fmap f . runJSONUpdateMonadT

instance (Applicative m, Monad m) => Applicative (JSONUpdateMonadT acid url session m) where
    pure = return
    l <*> r = JSONUpdateMonadT $ runJSONUpdateMonadT l <*> runJSONUpdateMonadT r
                           
instance Monad m => MonadJSONError (UpdateMonadT acid (EitherT Error (APIMonadT url session m))) where
    throwJSONError = lift . throwJSONError

instance Monad m => MonadJSONError (JSONUpdateMonadT acid url session m) where
    throwJSONError = JSONUpdateMonadT . lift . throwJSONError

instance Monad m => MonadUpdateError (JSONUpdateMonadT acid url session m) where
    throwUpdateError = JSONUpdateMonadT . lift . lift . throwUpdateError 

instance Monad m => MonadImageError (JSONUpdateMonadT acid url session m) where
    throwImageError = JSONUpdateMonadT . lift . lift . throwImageError 

instance Monad m => MonadJSON (JSONUpdateMonadT acid url session m) where
    maybeValue = JSONUpdateMonadT . maybeValue
    writeProp n = JSONUpdateMonadT . writeProp n
    extract = JSONUpdateMonadT . extract . runJSONUpdateMonadT
    useObject obj = JSONUpdateMonadT . useObject obj . runJSONUpdateMonadT

instance Monad m => MonadUpdate (JSONUpdateMonadT NoC url session m) where
    doLoginU l = JSONUpdateMonadT . lift . doLoginU l
    getOperatorIdU = JSONUpdateMonadT . lift $ getOperatorIdU 
    addAdminU = JSONUpdateMonadT . lift . addAdminU
    rmAdminU = JSONUpdateMonadT . lift . rmAdminU
    getChanNameU = JSONUpdateMonadT . lift . getChanNameU
    getChanDescU = JSONUpdateMonadT . lift . getChanDescU
    getChanTypeU = JSONUpdateMonadT . lift . getChanTypeU
    setChanNameU c = JSONUpdateMonadT . lift . setChanNameU c
    setChanDescU c = JSONUpdateMonadT . lift . setChanDescU c
    setChanTypeU c = JSONUpdateMonadT . lift . setChanTypeU c
    addChanOwnerU c = JSONUpdateMonadT . lift . addChanOwnerU c
    addChanProducerU c = JSONUpdateMonadT . lift . addChanProducerU c
    addChanConsumerU c = JSONUpdateMonadT . lift . addChanConsumerU c
    rmChanOwnerU c = JSONUpdateMonadT . lift . rmChanOwnerU c
    rmChanProducerU c = JSONUpdateMonadT . lift . rmChanProducerU c
    rmChanConsumerU c = JSONUpdateMonadT . lift . rmChanConsumerU c
    amountOfDistinctUsersU = JSONUpdateMonadT . lift . amountOfDistinctUsersU
    lastPostTimestampU = JSONUpdateMonadT . lift . lastPostTimestampU 
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
    addUserContactU u = JSONUpdateMonadT . lift . addUserContactU u
    rmUserContactU u = JSONUpdateMonadT . lift . rmUserContactU u
    getUserNotificationsU u o = JSONUpdateMonadT . lift . getUserNotificationsU u o
    addUserNotificationU u = JSONUpdateMonadT . lift . addUserNotificationU u
    tryToAddUserNotificationU u = JSONUpdateMonadT . lift . tryToAddUserNotificationU u
    getUserByLoginU = JSONUpdateMonadT . lift . getUserByLoginU
    createUserU l = JSONUpdateMonadT . lift . createUserU l
    createChannelU n = JSONUpdateMonadT . lift . createChannelU n
    postU c ts t = JSONUpdateMonadT . lift . postU c ts t
    messagesU c o = JSONUpdateMonadT . lift . messagesU c o
