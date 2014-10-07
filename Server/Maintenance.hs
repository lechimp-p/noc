{-# LANGUAGE TemplateHaskell #-}

module Maintenance where

import API.Config
import API.Session
import API.Effects
import API.Utils
import API (route, apiroutes, RootAPI (Default)) 
import API.Happstack
import Model
import Model.Acid

import System.Console.GetOpt
import System.Environment
import Control.Eff.Lift (runLift)
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Exception (bracket)
import Control.Lens
import Data.Yaml
import qualified Data.Aeson.TH as TH 
import Data.Acid (openLocalStateFrom)
import Data.Acid.Local (createCheckpointAndClose)
import Happstack.Server
        ( ServerPartT, Response
        )
import Happstack.Server.Internal.Cookie (CookieLife)
import Happstack.Server.ClientSession
        ( ClientSessionT
        , getKey, mkSessionConf
        , withClientSessionT, SessionConf
        )
import Web.Routes
import Web.Routes.Happstack (implSite)
import Web.Routes.Boomerang (boomerangSite)

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


$(TH.deriveJSON TH.defaultOptions{TH.fieldLabelModifier = drop 1} ''SessionConfig)
$(TH.deriveJSON TH.defaultOptions{TH.fieldLabelModifier = drop 1} ''SiteConfig)
$(TH.deriveJSON TH.defaultOptions{TH.fieldLabelModifier = drop 1} ''ImageConfig)
$(TH.deriveJSON TH.defaultOptions{TH.fieldLabelModifier = drop 1} ''BodyPolicy)
$(TH.deriveJSON TH.defaultOptions{TH.fieldLabelModifier = drop 1} ''ServerConfig)
$(TH.deriveJSON TH.defaultOptions ''CookieLife)
$(TH.deriveJSON TH.defaultOptions{TH.fieldLabelModifier = drop 1} ''APIConfig)
    
readConfig :: FilePath -> IO (Maybe APIConfig) 
readConfig path = do
    res <- decodeFileEither path
    case res of
        Left err -> do 
            putStrLn . show $ err
            return Nothing
        Right res -> return . Just $ res
    
withConfig :: FilePath -> (APIConfig -> IO a) -> IO (Maybe a)
withConfig path m = do
    cfg <- readConfig path
    case cfg of
        Nothing -> return Nothing
        Just c -> fmap Just (m c)

withACID :: FilePath -> NoC -> (AcidState NoC -> IO a) -> IO a
withACID path def m = 
    bracket (openLocalStateFrom path def)
            createCheckpointAndClose
            m
     
acidAPISite :: APIConfig -> AcidState NoC 
            -> Site RootAPI (ClientSessionT AuthData (ServerPartT IO) Response)
acidAPISite cfg acid = setDefault Default $ boomerangSite (const action) apiroutes
    where
    action :: RootAPI -> ClientSessionT AuthData (ServerPartT IO) Response
    action = fmap makeResponse 
           . unCSSPT
           . runLift 
           . runAPI cfg 
           . normalizeResponse
           . fmap (join . fmapl ModelError')
           . runAcid acid Nothing 
           . route

runAcidAPISite :: APIConfig -> AcidState NoC -> ServerPartT IO Response
runAcidAPISite cfg acid = do
    key <- liftIO . getKey $ cfg ^. sessionConfig . keyfileName
    let sessionConf = mkSessionConf key
    withClientSessionT sessionConf $
        let loc = cfg ^. siteConfig . location 
            path = cfg ^. siteConfig . handlerPath
        in implSite loc path $ acidAPISite cfg acid
