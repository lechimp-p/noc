{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Auth
where

import API.Effects
import API.Session
import API.Utils
import Model.BaseTypes
import Model.Exec
import Model.Constraints

import Data.Data (Data, Typeable)
import Data.Time.Clock (UTCTime)
import Control.Lens (makeLenses)
import Data.Aeson (Value)
import Control.Lens
import Control.Eff
import Control.Eff.JSON


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
          => Eff r (Either Error (Maybe Value))
logUserIn = withJSONIO $ do
        l <- makeLogin =<< prop "login" 
        pw <- makePassword =<< prop "password" 
        "id" <$ doLogin l pw
        refreshCookie (Just l) (Just pw)


logUserOut :: (Member API r, Member Exec r)
           => Eff r (Either Error (Maybe Value)) 
logUserOut = do
    refreshCookie Nothing Nothing
    return $ Right Nothing

refreshCookie :: (Member API r, Member Exec r)
              => Maybe Login -> Maybe Password -> Eff r () 
refreshCookie l pw = do
    ifIsJust l $ \ _ -> authSet (set login l)
    ifIsJust pw $ \ _ -> authSet (set password pw)


trySessionLogin :: (Member API r, Member Exec r)
                => Eff r () 
trySessionLogin = do
    l' <- authLogin
    pw' <- authPassword
    case (l', pw') of
        (Just l, Just pw) -> doLogin l pw >> return ()
        _ -> return ()
