{-# LANGUAGE DeriveGeneric #-}

module API.Channel
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
       ( ServerPartT, Response, ok, toResponse
       )

import Model.BaseTypes
import API.Monad

ok' = ok . toResponse . pack

data API
    = Get
    | Set
    deriving (Generic)

route :: ChanId -> API -> MonadAPI API Response
route uid url = case url of
    otherwise -> ok' $ "channel\n"
