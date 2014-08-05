{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.User 
where

import Data.Text (pack, Text)
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
        ( Response, ok, method
        , Method (POST, GET, HEAD)
        )
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative

import qualified Model.BaseTypes as BT
import API.Monad
import API.Utils
import API.Auth (AuthData, logUserIn)

data API
    = Login
    | Logout
    | Get
    | Set
    | UploadIcon
    | Contacts
    | Subscriptions
    | Channels
    deriving (Generic)

route :: BT.UserId -> API -> APIMonad API AuthData Response
route uid url = case url of
    Login           -> method [POST, HEAD] >> loginHandler uid 
    Logout          -> ok' "logout\n"
    Get             -> ok' "get\n"
    Set             -> ok' "set\n"
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> ok' "contacts\n"
    Subscriptions   -> ok' "subscriptions\n"
    Channels        -> ok' "channels\n"

--------
-- Login
--------

loginHandler :: BT.UserId -> APIMonad API AuthData Response
loginHandler _ = parseBody $ \obj -> do
    l <- obj .: "login"
    pw <- obj .: "password"
    return $ logUserIn l pw >> noContent'
