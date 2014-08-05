{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Web.Routes.Happstack (implSite)
import Happstack.Server 
        ( simpleHTTP, nullConf
        , decodeBody, BodyPolicy, defaultBodyPolicy
        )
import Happstack.Server.ClientSession
        ( getDefaultKey, mkSessionConf
        , withClientSessionT
        )
import Control.Exception (bracket)
import Data.Acid (openLocalState)
import Data.Acid.Local (createCheckpointAndClose)

import Model
import API (api)
import ACID

bodyPolicy = defaultBodyPolicy "/tmp/NoC-Server-dev"
                               1000 -- file upload
                               1000 -- no files
                               1000 -- overhead for multipart/form-data headers

initialNoC = mkNoC (mkLogin "admin") (mkPassword "admin") 

main :: IO ()
main = do
    key <- getDefaultKey
    let sessionConf = mkSessionConf key
    bracket (openLocalState initialNoC)
            createCheckpointAndClose 
        $ \acid ->
            simpleHTTP nullConf . withClientSessionT sessionConf $ do
                decodeBody bodyPolicy
                implSite "http://localhost:8000" "" api 
