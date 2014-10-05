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
import API.Errors
import Model.BaseTypes
import Model.Exec

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
          => Eff r Value 
logUserIn acid = handleError $ 
    queryWithJSONResponse acid $ do
        l <- prop "login"
        pw <- prop "password"
        "id" <$ doLogin l pw
        refreshCookie (Just l) (Just pw)


logUserOut :: (Member API r, Member Exec r)
           => Eff r () 
logUserOut = do
    refreshCookie Nothing Nothing
    noContent


refreshCookie :: (Member API r, Member Exec r)
              => Maybe Login -> Maybe Password -> m () 
refreshCookie l pw = do
    ifIsJust l $ \ _ -> authSet (set login l)
    ifIsJust pw $ \ _ -> authSet (set password pw)


trySessionLogin :: (Member API r, Member Exec r)
                => m () 
trySessionLogin = do
    l' <- authLogin
    pw' <- authPassword
    case (l', pw') of
        (Just l, Just pw) -> doLogin l pw >> return ()
        _ -> return ()
