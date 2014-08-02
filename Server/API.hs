{-# LANGUAGE TemplateHaskell #-}

module API
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.TH
import Web.Routes.Happstack
import Happstack.Server 
       (ServerPartT, Response, ok, toResponse)

import Model.BaseTypes
import qualified API.User as User
import qualified API.Chan as Chan

data API
    = User UserId User.API
    | Channel ChanId Chan.API
    | Default

route :: API -> RouteT API (ServerPartT IO) Response
route url = case url of
    User uid uapi       -> User uid `nestURL` User.route uid uapi 
    Channel cid capi    -> Channel cid `nestURL` Chan.route cid capi
    Default             -> helloWorld

api :: Site API (ServerPartT IO Response)
api = setDefault Default $ mkSitePI (runRouteT route)

helloWorld :: RouteT API (ServerPartT IO) Response
helloWorld = ok . toResponse . pack $ "This is the NoC-Server.\n"

$(derivePathInfo ''ChanId)
$(derivePathInfo ''UserId)
$(derivePathInfo ''User.API)
$(derivePathInfo ''Chan.API)
$(derivePathInfo ''API) 
