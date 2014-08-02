{-# LANGUAGE DeriveGeneric #-}

module API.Channel
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.TH
import Web.Routes.Happstack
import Happstack.Server 
       ( ServerPartT, Response, ok, toResponse
       )

import Model.BaseTypes

ok' = ok . toResponse . pack

data API
    = Get
    | Set
    deriving (Generic)

route :: ChanId -> API -> RouteT API (ServerPartT IO) Response
route uid url = case url of
    otherwise -> ok' $ "channel\n"
