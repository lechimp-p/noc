{-# LANGUAGE DeriveGeneric #-}

module API
where

import Data.Text (pack)
import Web.Routes
        ( Site, setDefault, PathInfo, Generic
        , mkSitePI, runRouteT )
import Web.Routes.Happstack
import Happstack.Server 
        ( ServerPartT, Response, ok
        , toResponse)

import Model.BaseTypes
import API.Monad
import API.Auth
import qualified API.User as User
import qualified API.Channel as Channel


data API
    = User Int User.API
    | Channel Int Channel.API
    | Default
    deriving (Generic)

route :: API -> MonadAPI API AuthData Response
route url = case url of
    User uid uapi       -> User uid `nestURL` User.route (UserId uid) uapi 
    Channel cid capi    -> Channel cid `nestURL` Channel.route (ChanId cid) capi
    Default             -> helloWorld

api :: Site API (InnerMonadAPI AuthData Response)
api = setDefault Default $ mkSitePI (runRouteT $ unMonadAPI . route)

helloWorld :: MonadAPI API AuthData Response
--helloWorld = ok . toResponse . pack $ "This is the NoC-Server.\n"
--helloWorld = showURL (User 100 User.Get) >>= ok . toResponse 
helloWorld = do
    lg <- authLogin
    pw <- authPassword
    timestamp <- authTimestamp
    ok . toResponse . pack $ show lg ++ " " ++ show pw ++ " " ++ show timestamp ++ "\n"

instance PathInfo User.API
instance PathInfo Channel.API
instance PathInfo API
