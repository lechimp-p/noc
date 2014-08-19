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

import API.APIMonad
import API.Errors
import API.JSONUtils
import ACID
import Model


newtype JSONQueryMonadT acid url session m a = 
    JSONQueryMonadT { runJSONQueryMonadT :: JSONMonadT (QueryMonadT acid (EitherT Error (APIMonadT url session m))) a}
    deriving (Monad, MonadIO, Functor, Applicative, MonadPlus)
                            
instance Monad m => MonadJSONError (QueryMonadT acid (EitherT Error (APIMonadT url session m))) where
    throwJSONError = lift . throwJSONError

instance Monad m => MonadJSONError (JSONQueryMonadT acid url session m) where
    throwJSONError = JSONQueryMonadT . lift . throwJSONError

instance Monad m => MonadQueryError (JSONQueryMonadT acid url session m) where
    throwQueryError = JSONQueryMonadT . lift . lift . throwQueryError 

instance Monad m => MonadJSON (JSONQueryMonadT acid url session m) where
    readProp = JSONQueryMonadT . readProp
    writeProp n = JSONQueryMonadT . writeProp n
    writeListProp n = JSONQueryMonadT . writeListProp n . fmap runJSONQueryMonadT 

instance Monad m => MonadQuery (JSONQueryMonadT NoC url session m) where
    doLoginQ l = JSONQueryMonadT . lift . doLoginQ l
    getOperatorIdQ = JSONQueryMonadT . lift $ getOperatorIdQ 
    getChanNameQ = JSONQueryMonadT . lift . getChanNameQ
    getChanDescQ = JSONQueryMonadT . lift . getChanDescQ
    getUserLoginQ = JSONQueryMonadT . lift . getUserLoginQ
    getUserNameQ = JSONQueryMonadT . lift . getUserNameQ
    getUserDescQ = JSONQueryMonadT . lift . getUserDescQ 
    getUserIconQ = JSONQueryMonadT . lift . getUserIconQ
    getUserOwnedChannelsQ = JSONQueryMonadT . lift . getUserOwnedChannelsQ 
    getUserSubscriptionsQ = JSONQueryMonadT . lift . getUserSubscriptionsQ
    getUserContactsQ = JSONQueryMonadT . lift . getUserContactsQ
    getUserByLoginQ = JSONQueryMonadT . lift . getUserByLoginQ
    messagesQ c o = JSONQueryMonadT . lift . messagesQ c o
