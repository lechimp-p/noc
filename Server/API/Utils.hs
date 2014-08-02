{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.Utils
where

import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.Text as T
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
     
ok' = ok . toResponse . T.pack
noContent' = noContent . toResponse . T.pack $ ""
badRequest' = badRequest . toResponse . T.pack
