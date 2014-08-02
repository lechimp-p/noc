{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Web.Routes.Happstack (implSite)
import Happstack.Server (simpleHTTP, nullConf)
import Happstack.Server.ClientSession
        ( getDefaultKey, mkSessionConf
        , withClientSessionT
        )

import API (api)

main :: IO ()
main = do
    key <- getDefaultKey
    let sessionConf = mkSessionConf key
    simpleHTTP nullConf $ withClientSessionT sessionConf 
                        $ implSite "http://localhost:8000" "" api 
