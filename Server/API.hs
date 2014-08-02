{-# LANGUAGE DeriveGeneric #-}

module API
where

import Data.Text (pack)
import Web.Routes
--import Web.Routes.TH
import Web.Routes.Happstack
import Happstack.Server 
       (ServerPartT, Response, ok, toResponse)

import Model.BaseTypes
import qualified API.User as User
import qualified API.Channel as Channel

data API
    = User Int User.API
    | Channel Int Channel.API
    | Default
    deriving (Generic)

route :: (Monad m) => API -> RouteT API (ServerPartT IO) Response
route url = case url of
    User uid uapi       -> User uid `nestURL` User.route (UserId uid) uapi 
    Channel cid capi    -> Channel cid `nestURL` Channel.route (ChanId cid) capi
    Default             -> helloWorld

api :: Site API (ServerPartT IO Response)
api = setDefault Default $ mkSitePI (runRouteT route)

helloWorld :: RouteT API (ServerPartT IO) Response
helloWorld = ok . toResponse . pack $ "This is the NoC-Server.\n"
--showURL (User 100 User.Get) >>= ok . toResponse 

instance PathInfo User.API
instance PathInfo Channel.API
instance PathInfo API
