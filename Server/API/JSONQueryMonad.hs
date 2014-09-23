{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module API.JSONQueryMonad
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
import ACID
import Model


newtype JSONQueryMonadT acid url session m a = 
    JSONQueryMonadT { runJSONQueryMonadT :: JSONMonadT (QueryMonadT acid (EitherT Error (APIMonadT url session m))) a}
    deriving (Monad, MonadIO, MonadPlus)

instance (Functor m, Monad m) => Functor (JSONQueryMonadT acid url session m) where
    fmap f = JSONQueryMonadT . fmap f . runJSONQueryMonadT

instance (Applicative m, Monad m) => Applicative (JSONQueryMonadT acid url session m) where
    pure = return
    l <*> r = JSONQueryMonadT $ runJSONQueryMonadT l <*> runJSONQueryMonadT r
                            
instance Monad m => MonadJSONError (QueryMonadT acid (EitherT Error (APIMonadT url session m))) where
    throwJSONError = lift . throwJSONError

instance Monad m => MonadJSONError (JSONQueryMonadT acid url session m) where
    throwJSONError = JSONQueryMonadT . lift . throwJSONError

instance Monad m => MonadQueryError (JSONQueryMonadT acid url session m) where
    throwQueryError = JSONQueryMonadT . lift . lift . throwQueryError 

instance Monad m => MonadJSON (JSONQueryMonadT acid url session m) where
    maybeValue = JSONQueryMonadT . maybeValue
    writeProp n = JSONQueryMonadT . writeProp n
    extract = JSONQueryMonadT . extract . runJSONQueryMonadT
    useObject obj = JSONQueryMonadT . useObject obj . runJSONQueryMonadT

instance Monad m => MonadQuery (JSONQueryMonadT NoC url session m) where
    doLoginQ l = JSONQueryMonadT . lift . doLoginQ l
    getOperatorIdQ = JSONQueryMonadT . lift $ getOperatorIdQ 
    getChanNameQ = JSONQueryMonadT . lift . getChanNameQ
    getChanDescQ = JSONQueryMonadT . lift . getChanDescQ
    getChanTypeQ = JSONQueryMonadT . lift . getChanTypeQ
    amountOfDistinctUsersQ = JSONQueryMonadT . lift . amountOfDistinctUsersQ
    lastPostTimestampQ = JSONQueryMonadT . lift . lastPostTimestampQ 
    getUserLoginQ = JSONQueryMonadT . lift . getUserLoginQ
    getUserNameQ = JSONQueryMonadT . lift . getUserNameQ
    getUserDescQ = JSONQueryMonadT . lift . getUserDescQ 
    getUserIconQ = JSONQueryMonadT . lift . getUserIconQ
    getUserOwnedChannelsQ = JSONQueryMonadT . lift . getUserOwnedChannelsQ 
    getUserSubscriptionsQ = JSONQueryMonadT . lift . getUserSubscriptionsQ
    getUserNotificationsQ u o = JSONQueryMonadT . lift . getUserNotificationsQ u o
    getUserContactsQ = JSONQueryMonadT . lift . getUserContactsQ
    getUserByLoginQ = JSONQueryMonadT . lift . getUserByLoginQ
    messagesQ c o = JSONQueryMonadT . lift . messagesQ c o
    messagesTillQ c = JSONQueryMonadT . lift . messagesTillQ c
