{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Maintenance
import API.Config
import Model
import Model.Acid

import Web.Routes.Happstack (implSite)
import Happstack.Server (simpleHTTP, nullConf)

initialNoC = mkNoC (mkLogin "admin") (mkPassword "admin") 

main :: IO Int 
main = do
    opts <- readOptions
    withConfig (optConfigFile opts) $ \ cfg -> do
        withACID (_acidPath cfg) initialNoC $ \acid -> do
            simpleHTTP nullConf $ runAcidAPISite cfg acid
    return 0
