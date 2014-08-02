module API
where

import Web.Routes
import Web.Routes.TH
import Web.Routes.Happstack
import Happstack.Server 
       (ServerPartT, Response)

import Model.BaseTypes
import qualified API.User as User
import qualified API.Chan as Chan

data API
    = User UserId User.API
    | Channel ChanId Chan.API

route :: API -> RouteT API (ServerPartT IO) Response
route url = case url of
    User uid uapi -> User uid `nestURL` User.route uid uapi 
    Channel cid capi -> Channel cid `nestURL` Chan.route cid capi

$(derivePathInfo ''ChanId)
$(derivePathInfo ''UserId)
$(derivePathInfo ''User.API)
$(derivePathInfo ''Chan.API)
$(derivePathInfo ''API) 
