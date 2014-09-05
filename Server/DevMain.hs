{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Web.Routes.Happstack (implSite)
import Happstack.Server 
        ( simpleHTTP, nullConf, Response
        , decodeBody, BodyPolicy, defaultBodyPolicy
        )
import Happstack.Server.ClientSession
        ( getDefaultKey, mkSessionConf
        , withClientSessionT, SessionConf
        )
import Happstack.Server.Internal.Cookie (CookieLife (..))
import Happstack.Server.Monads
import Control.Exception (bracket)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Acid (openLocalState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Text (Text)

import Model
import Maintenance
import API (site)
import API.Config
import API.Utils (ACID)
import API.Auth (AuthData)
import API.APIMonad (InnerAPIMonadT)
import ACID.Query
import ACID.Update

initialNoC = mkNoC (mkLogin "admin") (mkPassword "admin") 

main :: IO ()
main = do
    opts <- readOptions
    cfg <- readConfig . optConfigFile $ opts 

    case cfg of
        Nothing -> return ()
        Just cfg -> do
            bracket (openLocalState initialNoC)
                    createCheckpointAndClose 
                    $ \acid ->
                        simpleHTTP nullConf $ site cfg acid
