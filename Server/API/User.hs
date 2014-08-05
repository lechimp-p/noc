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
import API.ACIDEvents
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

route :: ACID -> BT.UserId -> API -> APIMonad API AuthData Response
route acid uid url = case url of
    Login           -> method [POST, HEAD] >> loginHandler acid uid 
    Logout          -> ok' "logout\n"
    Get             -> method [GET, HEAD] >> getHandler acid uid 
    Set             -> ok' "set\n"
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> ok' "contacts\n"
    Subscriptions   -> ok' "subscriptions\n"
    Channels        -> ok' "channels\n"


loginHandler acid _ = parseBody $ \obj -> do
    l <- obj .: "login"
    pw <- obj .: "password"
    return $ logUserIn acid l pw 

getHandler acid uid = error "foo" 
