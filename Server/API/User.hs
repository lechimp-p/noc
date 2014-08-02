module API.User 
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.TH
import Web.Routes.Happstack
import Happstack.Server 
       ( ServerPartT, Response, ok, toResponse
       )

import Model.BaseTypes

data API
    = Login
    | Get
    | Set
    | UploadIcon
    | Contacts
    | Subscriptions
    | Channels

route :: UserId -> API -> RouteT API (ServerPartT IO) Response
route uid url = case url of
    otherwise -> ok . toResponse . pack $ "user"

