{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module API.Channel
where

import Data.Text (pack)
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
       ( ServerPartT, Response, ok, toResponse
       , FilterMonad
       )

import Model.BaseTypes
import API.ACIDEvents
import API.Monad

ok' = ok . toResponse . pack

data API
    = Get
    | Set
    deriving (Generic)

route :: FilterMonad Response m
      => ACID -> ChanId -> API -> APIMonadT API s m Response
route acid uid url = case url of
    otherwise -> ok' $ "channel\n"
