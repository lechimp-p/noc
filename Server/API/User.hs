{-# LANGUAGE DeriveGeneric #-}

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

route :: BT.UserId -> API -> MonadAPI API Response
route uid url = case url of
    Login           -> ok' "login\n"
    Get             -> ok' "get\n"
    Set             -> ok' "set\n"
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> ok' "contacts\n"
    Subscriptions   -> ok' "subscriptions\n"
    Channels        -> ok' "channels\n"

