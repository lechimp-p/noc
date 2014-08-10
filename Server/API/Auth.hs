{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
        )
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Acid.Advanced ( query' )

import API.APIMonad
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

authGet :: (Monad m, MonadIO m, Functor m)
        => (AuthData -> a) -> APIMonadT url AuthData m a
authGet f = getSession >>= return . f 

authSet :: (Monad m, MonadIO m, Functor m)
        => (AuthData -> AuthData) -> APIMonadT url AuthData m ()
authSet f = getSession >>= putSession . f

authPassword :: (Monad m, MonadIO m, Functor m)
             => APIMonadT url AuthData m (Maybe Password)
authPassword = authGet _password

authLogin :: (Monad m, MonadIO m, Functor m)
          => APIMonadT url AuthData m (Maybe Login)
authLogin = authGet _login

authTimestamp :: (Monad m, MonadIO m, Functor m)
              => APIMonadT url AuthData m (Maybe UTCTime)
authTimestamp = authGet _timestamp

logUserIn :: (Monad m, MonadIO m, Functor m) 
          => ACID -> Login -> Password -> APIMonadT url AuthData m Response
logUserIn acid l pw = handleError $ 
    flip runQueryMonadT acid $ do
        doLoginQ l pw
        lift $ refreshCookie (Just l) (Just pw)
        noContentQ

logUserOut :: (Monad m, MonadIO m, Functor m) 
           => APIMonadT url AuthData m Response
logUserOut = do
    refreshCookie Nothing Nothing
    noContent'

refreshCookie :: (Monad m, MonadIO m, Functor m) 
              => Maybe Login -> Maybe Password -> APIMonadT url AuthData m () 
refreshCookie l pw = do
    ifIsJust l $ \ _ -> authSet (set login l)
    ifIsJust pw $ \ _ -> authSet (set password pw)

{--
withAuth :: (MonadIO m, FilterMonad Response m, Functor m)
         => (Login -> Password -> Transaction (Either Error a) NoC)
         -> APIMonadT url AuthData m (Maybe (Transaction (Either Error a) NoC))
withAuth tr = do
    l <- authGet _login
    pw <- authGet _password
    case (l, pw) of
        (Just l', Just pw') -> return . Just $ tr l' pw'
        otherwise           -> return Nothing 
--}

runHandler :: (Monad m, MonadIO m)
           => ACID
           -> (a -> APIMonadT url AuthData m Response)
           -> APIMonadT url AuthData m Response
runHandler acid op = error "TODO" 
{--do
    ta <- withAuth tr
    case ta of
        (Just ta')  -> handleErrors acid ta' op 
        Nothing     -> respondError NotLoggedIn  
--}

