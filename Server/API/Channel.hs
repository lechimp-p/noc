{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.Channel
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
       ( ServerPartT, Response, ok, toResponse
       )

import Model.BaseTypes
import API.ACIDEvents
import API.Monad

ok' = ok . toResponse . pack

data API
    = Get
    | Set
    deriving (Generic)

route :: ACID -> ChanId -> API -> APIMonad API s Response
route acid uid url = case url of
    otherwise -> ok' $ "channel\n"
