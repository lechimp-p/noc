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
import Control.Monad.IO.Class

import Model.BaseTypes
import API.APIMonad
import API.Utils

data API
    = Get   -- name, desc, number of members, time of last post
    | Set
    | Messages -- name of author, timestamp, text, image
    | Post -- text, image
    | Subscribe
    | Unsubscribe
    deriving (Generic)

route :: (Monad m, MonadIO m)
      => ACID -> ChanId -> API -> APIMonadT API s m Response
route acid uid url = case url of
    otherwise -> ok' $ "channel\n"
