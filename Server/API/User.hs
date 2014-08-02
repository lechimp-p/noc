module API.User 
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.TH
import Web.Routes.Happstack
import Happstack.Server 
       ( ServerPartT, Response, ok, toResponse
       )

import qualified Model.BaseTypes as BT

ok' = ok . toResponse . pack

data API
    = Login
    | Get
    | Set
    | UploadIcon
    | Contacts
    | Subscriptions
    | Channels

route :: BT.UserId -> API -> RouteT API (ServerPartT IO) Response
route uid url = case url of
    Login           -> ok' "login\n"
    Get             -> ok' "get\n"
    Set             -> ok' "set\n"
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> ok' "contacts\n"
    Subscriptions   -> ok' "subscriptions\n"
    Channels        -> ok' "channels\n"

