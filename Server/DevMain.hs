{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Maintenance
import API.Config
import Model
import Model.Acid

import Control.Lens
import Web.Routes.Happstack (implSite)
import Happstack.Server (simpleHTTP, nullConf, Conf (..))

initialNoC = mkNoC (mkLogin "admin") (mkPassword "admin") 

main :: IO Int 
main = do
    opts <- readOptions
    withConfig (optConfigFile opts) $ \ cfg -> do
        withACID (_acidPath cfg) initialNoC $ \acid -> do
            simpleHTTP (nullConf { Happstack.Server.port = cfg ^. serverConfig . API.Config.port
                                 , timeout = cfg ^. serverConfig . threadTimeout
                                 }
                       )
                       $ runAcidAPISite cfg acid
    return 0
