{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Utils
where

import Data.Acid ( AcidState )
import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString.Char8 as B 
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Happstack.Server
import Happstack.Server.Types
import Happstack.Server.ClientSession
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class

import Model
import API.Monad
import ACID.QueryMonad

type ACID = AcidState NoC

getBody :: (Monad m, MonadIO m) 
        => APIMonadT url session m L.ByteString
getBody = do
    body <- APIMonadT $ askRq >>= liftIO . takeRequestBody
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing -> return ""

parseBody :: (Monad m, MonadIO m) 
          => (Object -> Parser (APIMonadT url session m Response)) -> APIMonadT url session m Response 
parseBody parser = do
    body <- getBody
    let maybeAction = decode body >>= parseMaybe parser
    case maybeAction of
        Just action -> action
        Nothing -> badRequest' "Could not decode body."
     
ok' :: (Monad m, MonadIO m)
    =>  String -> APIMonadT url session m Response
ok' = ok . toResponse . T.pack

noContent' :: (Monad m, MonadIO m)
           => APIMonadT url session m Response
noContent' = noContent . toResponse . T.pack $ ""

badRequest' :: (Monad m, MonadIO m)
            =>  String -> APIMonadT url session m Response
badRequest' = badRequest . toResponse . T.pack


type APIQueryMonadT url session m a = QueryMonadT NoC (APIMonadT url session m) a

okQ :: (Monad m, MonadIO m)
    => String -> APIQueryMonadT url session m Response
okQ = lift . ok'

noContentQ :: (Monad m, MonadIO m) 
           => APIQueryMonadT url session m Response
noContentQ = lift noContent'

badRequestQ :: (Monad m, MonadIO m) 
            => String -> APIQueryMonadT url session m Response
badRequestQ = lift . badRequest'

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
