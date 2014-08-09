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
import Data.Acid.Advanced ( query' )

import API.ACIDEvents
import API.Monad
import API.Utils
import API.Errors
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


authGet :: (MonadIO m, FilterMonad Response m, Functor m)
        => (AuthData -> a) -> APIMonadT url AuthData m a
authGet f = getSession >>= return . f 

authSet :: (MonadIO m, FilterMonad Response m, Functor m)
        => (AuthData -> AuthData) -> APIMonadT url AuthData m ()
authSet f = getSession >>= putSession . f

authPassword :: (MonadIO m, FilterMonad Response m, Functor m)
             => APIMonadT url AuthData m (Maybe Password)
authPassword = authGet _password

authLogin :: (MonadIO m, FilterMonad Response m, Functor m)
          => APIMonadT url AuthData m (Maybe Login)
authLogin = authGet _login

authTimestamp :: (MonadIO m, FilterMonad Response m, Functor m)
              => APIMonadT url AuthData m (Maybe UTCTime)
authTimestamp = authGet _timestamp

logUserIn :: (MonadIO m, FilterMonad Response m) 
          => ACID -> Login -> Password -> APIMonadT url AuthData m Response
logUserIn acid l pw = error "TODO" 
{--    let ta = QueryTA $ LoginTA l pw  
    in handleErrors acid ta $ \ uid -> do 
            refreshCookie (Just l) (Just pw)
            noContent'
--}

logUserOut :: (MonadIO m, FilterMonad Response m, Functor m) 
           => APIMonadT url AuthData m Response
logUserOut = do
    refreshCookie Nothing Nothing
    noContent'

refreshCookie :: (MonadIO m, FilterMonad Response m, Functor m) 
              => Maybe Login -> Maybe Password -> APIMonadT url AuthData m () 
refreshCookie l pw = do
    ifIsJust l $ \ _ -> authSet (set login l)
    ifIsJust pw $ \ _ -> authSet (set password pw)

withAuth :: (MonadIO m, FilterMonad Response m, Functor m)
         => (Login -> Password -> Transaction (Either Error a) NoC)
         -> APIMonadT url AuthData m (Maybe (Transaction (Either Error a) NoC))
withAuth tr = do
    l <- authGet _login
    pw <- authGet _password
    case (l, pw) of
        (Just l', Just pw') -> return . Just $ tr l' pw'
        otherwise           -> return Nothing 

runHandler :: (MonadIO m, FilterMonad Response m)
           => ACID
           -> (Login -> Password -> Transaction (Either Error a) NoC)
           -> (a -> APIMonadT url AuthData m Response)
           -> APIMonadT url AuthData m Response
runHandler acid tr op = error "TODO" 
{--do
    ta <- withAuth tr
    case ta of
        (Just ta')  -> handleErrors acid ta' op 
        Nothing     -> respondError NotLoggedIn  
--}

