{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Auth
where

import API.Effects
import Model.Exec

--import API.Errors
--import Model.Errors

--import Data.Text
import Data.Data (Data, Typeable)
import Data.Time.Clock (UTCTime)
import Control.Lens (makeLenses)
--import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
--import Happstack.Server
--        ( Response, FilterMonad )
--import Happstack.Server.ClientSession 
--        ( ClientSession, emptySession
--        , getSession, putSession, expireSession 
--        , MonadClientSession
--        )
--import Data.Aeson
import Control.Lens
import Control.Eff
--import Control.Monad
--import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
--import Data.Acid.Advanced ( query' )
--import Control.Monad.Trans.JSON

{--data AuthData = AuthData 
    { _login        :: Maybe Login 
    , _password     :: Maybe Password 
    , _timestamp    :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Typeable)

-- $(deriveSafeCopy 0 'base ''AuthData)
makeLenses ''AuthData
--}
-- instance ClientSession AuthData where
--    emptySession = AuthData Nothing Nothing Nothing 


authGet :: (Member API r)
        => (AuthData -> a) -> Eff r a
authGet f = getSession >>= return . f 

authSet :: (Member API r)
        => (AuthData -> AuthData) -> Eff r ()
authSet f = getSession >>= putSession . f

authPassword :: (Member API r)
             => Eff r (Maybe Password)
authPassword = authGet _password

authLogin :: (Member API r)
          => Eff r (Maybe Login)
authLogin = authGet _login

authTimestamp :: (Member API r)
              => Eff r (Maybe UTCTime)
authTimestamp = authGet _timestamp

logUserIn :: (Member API r, Member Exec r)
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
