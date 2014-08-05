{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.Utils
where

import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Happstack.Server
import Happstack.Server.Types
import Control.Monad.IO.Class (liftIO)

import API.Monad

getBody :: APIMonad url session L.ByteString
getBody = do
    body <- APIMonad $ askRq >>= liftIO . takeRequestBody
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing -> return ""

parseBody :: (Object -> Parser (APIMonad url session Response)) -> APIMonad url session Response 
parseBody parser = do
    body <- getBody
    let maybeAction = decode body >>= parseMaybe parser
    case maybeAction of
        Just action -> action
        Nothing -> badRequest' "Could not decode body."
     
ok' = ok . toResponse . T.pack
noContent' = noContent . toResponse . T.pack $ ""
badRequest' = badRequest . toResponse . T.pack
