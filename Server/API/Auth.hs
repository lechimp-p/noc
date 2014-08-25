{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Auth
where

import Data.Text
import Data.Data (Data, Typeable)
import Data.Time.Clock
import Control.Lens (makeLenses)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Happstack.Server
        ( Response, FilterMonad )
import Happstack.Server.ClientSession 
        ( ClientSession, emptySession
        , getSession, putSession, expireSession 
        , MonadClientSession
        )
import Data.Aeson
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Acid.Advanced ( query' )
import Control.Monad.Trans.JSON

import API.APIMonad
import API.JSONQueryMonad
import API.Utils
import API.Errors
import ACID
import Model
import Model.Errors

data AuthData = AuthData 
    { _login        :: Maybe Login 
    , _password     :: Maybe Password 
    , _timestamp    :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''AuthData)
makeLenses ''AuthData

instance ClientSession AuthData where
    emptySession = AuthData Nothing Nothing Nothing 

authGet :: (Monad m, MonadIO m, Functor m, MonadClientSession AuthData m)
        => (AuthData -> a) -> m a
authGet f = getSession >>= return . f 

authSet :: (Monad m, MonadIO m, Functor m, MonadClientSession AuthData m)
        => (AuthData -> AuthData) -> m ()
authSet f = getSession >>= putSession . f

authPassword :: (Monad m, MonadIO m, Functor m, MonadClientSession AuthData m)
             => m (Maybe Password)
authPassword = authGet _password

authLogin :: (Monad m, MonadIO m, Functor m, MonadClientSession AuthData m)
          => m (Maybe Login)
authLogin = authGet _login

authTimestamp :: (Monad m, MonadIO m, Functor m, MonadClientSession AuthData m)
              => m (Maybe UTCTime)
authTimestamp = authGet _timestamp

logUserIn :: ( Monad m, MonadIO m, Functor m ) 
          => ACID -> APIMonadT url AuthData m Response
logUserIn acid = handleError $ 
    queryWithJSONResponse acid $ do
        l <- prop "login"
        pw <- prop "password"
        "id" <$ doLoginQ l pw
        refreshCookie (Just l) (Just pw)

logUserOut :: ( Monad m, MonadIO m, Functor m
              , MonadClientSession AuthData m
              , FilterMonad Response m
              ) 
           => m Response
logUserOut = do
    refreshCookie Nothing Nothing
    noContent'

refreshCookie :: (Monad m, MonadIO m, Functor m, MonadClientSession AuthData m) 
              => Maybe Login -> Maybe Password -> m () 
refreshCookie l pw = do
    ifIsJust l $ \ _ -> authSet (set login l)
    ifIsJust pw $ \ _ -> authSet (set password pw)

trySessionLoginQ :: ( Monad m, MonadIO m, Functor m
                    , MonadClientSession AuthData m
                    , MonadQuery m)
                 => m () 
trySessionLoginQ = do
    l' <- authLogin
    pw' <- authPassword
    case (l', pw') of
        (Just l, Just pw) -> doLoginQ l pw >> return ()
        _ -> return ()

trySessionLoginU :: ( Monad m, MonadIO m, Functor m
                    , MonadClientSession AuthData m
                    , MonadUpdate m)
                 => m () 
trySessionLoginU = do
    l' <- authLogin
    pw' <- authPassword
    case (l', pw') of
        (Just l, Just pw) -> doLoginU l pw >> return ()
        _ -> return ()
