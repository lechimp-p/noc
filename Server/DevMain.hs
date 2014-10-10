{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Maintenance
import API.Config
import Model
import Model.Acid
import Model.BaseTypes

import Data.Time.Format
import System.Locale
import Control.Lens
import Web.Routes.Happstack (implSite)
import Happstack.Server (simpleHTTP, nullConf, Conf (..))

initialNoC = mkNoC (Login "admin") (Password "admin") 

accessLog :: FormatTime time
          => String -> String -> time -> String -> Int -> Integer -> String -> String -> IO ()
accessLog host user time request response size referer useragent =
    putStrLn $ formatTime defaultTimeLocale "%T" time 
               ++ " - " ++ request ++ " : " ++ show response

main :: IO Int 
main = do
    opts <- readOptions
    withConfig (optConfigFile opts) $ \ cfg -> do
        withACID (_acidPath cfg) initialNoC $ \acid -> do
            simpleHTTP (nullConf { Happstack.Server.port = cfg ^. serverConfig . API.Config.port
                                 , timeout = cfg ^. serverConfig . threadTimeout
                                 , logAccess = Just accessLog 
                                 }
                       )
                       $ runAcidAPISite cfg acid
    return 0
