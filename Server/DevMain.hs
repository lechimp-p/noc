{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Web.Routes.Happstack (implSite)
import Happstack.Server (simpleHTTP, nullConf)

import API (api)

main :: IO ()
main = simpleHTTP nullConf $ implSite "http://localhost:8000" "" api
