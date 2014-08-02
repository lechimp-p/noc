{-# LANGUAGE DeriveGeneric #-}
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

route :: BT.UserId -> API -> MonadAPI API AuthData Response
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

data LoginJSON = LoginJson
    { login :: Text
    , password :: Text
    }
    deriving (Generic)

instance FromJSON LoginJSON

loginHandler :: BT.UserId -> MonadAPI API AuthData Response
loginHandler _ = do
    ld <- decode <$> getBody 
    case ld :: Maybe LoginJSON of
        Just ld' -> logUserIn (login ld') (password ld') >> noContent'
        Nothing -> badRequest' "Could not decode."

