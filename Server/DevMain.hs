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

import API (api)

bodyPolicy = defaultBodyPolicy "/tmp/NoC-Server-dev"
                               1000 -- file upload
                               1000 -- no files
                               1000 -- overhead for multipart/form-data headers

main :: IO ()
main = do
    key <- getDefaultKey
    let sessionConf = mkSessionConf key
    simpleHTTP nullConf . withClientSessionT sessionConf $ do
        decodeBody bodyPolicy
        implSite "http://localhost:8000" "" api 
