{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module API.Utils
where

import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString.Char8 as B 
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Happstack.Server
import Happstack.Server.Types
import Control.Monad.IO.Class (liftIO, MonadIO)

import API.Monad

getBody :: (Monad m, FilterMonad Response m, MonadIO m) 
        => APIMonadT url session m L.ByteString
getBody = do
    body <- APIMonadT $ askRq >>= liftIO . takeRequestBody
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing -> return ""

parseBody :: (Monad m, FilterMonad Response m, MonadIO m) 
          => (Object -> Parser (APIMonadT url session m Response)) -> APIMonadT url session m Response 
parseBody parser = do
    body <- getBody
    let maybeAction = decode body >>= parseMaybe parser
    case maybeAction of
        Just action -> action
        Nothing -> badRequest' "Could not decode body."
     
ok' = ok . toResponse . T.pack
noContent' = noContent . toResponse . T.pack $ ""
badRequest' = badRequest . toResponse . T.pack

instance ToMessage Value where
    toContentType _ = B.pack "application/json"
    toMessage       = encode

jsonR' :: FilterMonad Response m => Value -> m Response
jsonR' = ok . toResponse 

ifIsJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
ifIsJust v op =
    case v of
        Just a -> op a
        Nothing -> return () 
