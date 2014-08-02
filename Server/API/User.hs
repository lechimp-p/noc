{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.User 
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
       ( ServerPartT, Response, ok, toResponse
       )

import qualified Model.BaseTypes as BT
import API.Monad
import API.Auth (AuthData, logUserIn)

ok' = ok . toResponse . pack

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

route :: BT.UserId -> API -> MonadAPI API AuthData Response
route uid url = case url of
    Login           -> login uid 
    Logout          -> ok' "logout\n"
    Get             -> ok' "get\n"
    Set             -> ok' "set\n"
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> ok' "contacts\n"
    Subscriptions   -> ok' "subscriptions\n"
    Channels        -> ok' "channels\n"

login :: BT.UserId -> MonadAPI API AuthData Response
login _ = logUserIn >> ok' "logged in"
