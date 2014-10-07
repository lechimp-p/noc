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
import Control.Monad.Trans.Reader (runReaderT)
import Data.Acid (openLocalState)
import Data.Text (Text)

import Model
import Maintenance
import API (site)
import API.Config
import API.Utils (ACID)
import API.Auth (AuthData)
import API.APIMonad (InnerAPIMonadT)
import Model
import Model.Acid

initialNoC = mkNoC (mkLogin "admin") (mkPassword "admin") 


main :: IO ()
main = do
    opts <- readOptions
    cfg <- readConfig . optConfigFile $ opts 
    withConfig (optConfigFile opts) $ \ cfg -> do
        withACID (_acidPath cfg) initialNoC $ \acid -> do
            simpleHTTP nullConf $ site cfg acid

