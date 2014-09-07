{-# LANGUAGE TemplateHaskell #-}

module Maintenance 
    ( Options (..)
    , readOptions
    )
where

import System.Console.GetOpt
import System.Environment
import Data.Yaml
import qualified Data.Aeson.TH as TH 
import Happstack.Server.Internal.Cookie

import API.Config

data Options = Options
    { optConfigFile    :: FilePath
    }
    deriving (Show, Read)

options :: [ OptDescr (Options -> Options) ]
options = 
    [ Option "c" ["config"]
        (ReqArg (\arg opt -> opt { optConfigFile = arg }) "FILE")
        "Config file to be used."
    ]

defaultOptions = Options "conf.yaml"

readOptions :: IO Options
readOptions = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    return $ (foldr (.) id actions) defaultOptions


$(TH.deriveJSON TH.defaultOptions ''SessionConfig)
$(TH.deriveJSON TH.defaultOptions ''SiteConfig)
$(TH.deriveJSON TH.defaultOptions ''ImageConfig)
$(TH.deriveJSON TH.defaultOptions ''BodyPolicy)
$(TH.deriveJSON TH.defaultOptions ''CookieLife)
$(TH.deriveJSON TH.defaultOptions ''Config)
    
loadConfig :: FilePath -> IO (Maybe Config) 
loadConfig path = do
    res <- decodeFileEither path
    case res of
        Left err -> do 
            putStrLn . show $ err
            return Nothing
        Right res -> return . Just $ res
    

