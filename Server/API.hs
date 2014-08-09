{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module API
where

import Data.Text (pack)
import Web.Routes
        ( Site, setDefault, PathInfo, Generic
        , mkSitePI, runRouteT )
import Web.Routes.Happstack
import Happstack.Server 
        ( ServerPartT, Response, ok
        , toResponse, Method (..), method
        , FilterMonad
        )
import Data.Aeson
import Data.Aeson.Types
import Control.Monad.IO.Class

import qualified Model.BaseTypes as BT
import API.ACIDEvents (ACID)
import API.Monad
import API.Auth
import API.Utils
import qualified API.User as User
import qualified API.Channel as Channel


data API
    = Login 
    | Logout
    | User Int User.API
    | Channel Int Channel.API
    | Default
    deriving (Generic)

route :: (Functor m, Monad m, MonadIO m, FilterMonad Response m)
      => ACID -> API -> APIMonadT API AuthData m Response
route acid url = case url of
    Login               -> (>>) (method [POST, HEAD])
                           $ parseBody $ \obj -> do
                                l <- obj .: "login"
                                pw <- obj .: "password"
                                return $ logUserIn acid (BT.mkLogin l) (BT.mkPassword pw)
    Logout              -> method [POST, HEAD] 
                           >> logUserOut  
    User uid uapi       -> User uid `nestURL` User.route acid (BT.UserId uid) uapi 
    Channel cid capi    -> Channel cid `nestURL` Channel.route acid (BT.ChanId cid) capi
    Default             -> helloWorld


api :: (Functor m, Monad m, MonadIO m, FilterMonad Response m)
    => ACID -> Site API (InnerAPIMonadT AuthData m Response)
api acid = setDefault Default $ mkSitePI (runRouteT $ unAPIMonadT . route acid)

helloWorld :: (Functor m, Monad m, MonadIO m, FilterMonad Response m)
           => APIMonadT API AuthData m Response
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
