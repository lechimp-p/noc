{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module API
where

import Prelude hiding ( id, (.) )
import Control.Category ( Category(id, (.)) )
import Data.Text (pack)
import Web.Routes
        ( Site, setDefault, PathInfo, Generic
        , mkSitePI, runRouteT, RouteT )
import Web.Routes.Happstack
import Web.Routes.Boomerang
import Happstack.Server 
        ( ServerPartT, Response, ok
        , toResponse, Method (..), method
        , FilterMonad
        )
import Happstack.Server.ClientSession
        ( getKey, mkSessionConf
        , withClientSessionT, SessionConf
        )
import Data.Aeson
import Data.Aeson.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (runReaderT)
import Text.Boomerang.TH (makeBoomerangs)
import Control.Lens

import qualified Model.BaseTypes as BT
import API.APIMonad
import API.Auth
import API.Config
import API.Utils
import qualified API.User as User
import qualified API.Channel as Channel


data API
    = Login 
    | Logout
    | User Int User.API
    | UserGeneric
    | Channel Int Channel.API
    | ChannelGeneric 
    | Default
    deriving (Generic)

$(makeBoomerangs ''API)

apiroutes :: Router () (API :- ())
apiroutes = 
    (  "login" . rLogin
    <> "logout" . rLogout
    <> "user" . user
    <> "channel" . channel
    <> rDefault
    )
    where
    user =      rUserGeneric
             <> rUser </> int </> User.userroutes 
    channel =   rChannelGeneric 
             <> rChannel </> int </> Channel.channelroutes


route :: (Functor m, Monad m, MonadIO m)
      => ACID -> API -> APIMonadT API AuthData m Response
route acid url = case url of
    Login               -> (method [POST, HEAD])
                           >> logUserIn acid
    Logout              -> method [POST, HEAD] 
                           >> logUserOut  
    User uid uapi       -> User uid `nestURL` User.route acid (BT.UserId uid) uapi 
    UserGeneric         -> User.genericHandler acid
    Channel cid capi    -> Channel cid `nestURL` Channel.route acid (BT.ChanId cid) capi
    ChannelGeneric      -> Channel.genericHandler acid
    Default             -> helloWorld

api :: (Functor m, Monad m, MonadIO m)
    => Config -> ACID -> Site API (InnerAPIMonadT AuthData m Response)
api cfg acid = 
    let handler :: (MonadIO m, Functor m) => API -> RouteT API (InnerAPIMonadT AuthData m) Response
        handler = flip runReaderT cfg . unAPIMonadT . route acid
    in setDefault Default $ boomerangSite (runRouteT handler) apiroutes
--api acid = setDefault Default $ mkSitePI (runRouteT $ unAPIMonadT . route acid)

site :: Config -> ACID -> ServerPartT IO Response
site cfg acid = do
    key <- liftIO . getKey $ cfg ^. sessionConfig . keyfileName
    let sessionConf = mkSessionConf key
    withClientSessionT sessionConf $
        implSite (cfg ^. siteConfig . location) (cfg ^. siteConfig . handlerPath) $
        api cfg acid

helloWorld :: (Functor m, Monad m, MonadIO m)
           => APIMonadT API AuthData m Response
helloWorld = ok . toResponse =<< config helloWorldMessage

instance PathInfo User.API
instance PathInfo Channel.API
instance PathInfo API
