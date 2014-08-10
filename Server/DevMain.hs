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
import Happstack.Server.Monads
import Control.Exception (bracket)
import Data.Acid (openLocalState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Text (Text)

import Model
import API (api)
import API.Utils (ACID)
import API.Auth (AuthData)
import API.Monad (InnerAPIMonadT)
import ACID.Query
import ACID.Update

bodyPolicy = defaultBodyPolicy "/tmp/NoC-Server-dev"
                               1000 -- file upload
                               1000 -- no files
                               1000 -- overhead for multipart/form-data headers

initialNoC = mkNoC (mkLogin "admin") (mkPassword "admin") 

main :: IO ()
main = do
    key <- getDefaultKey
    let sessionConf = mkSessionConf key
    bracket (openLocalState initialNoC)
            createCheckpointAndClose 
        $ \acid ->
            simpleHTTP nullConf 
                $ site' sessionConf "http://localhost:8000" "" acid

site :: Text -> Text -> ACID -> InnerAPIMonadT AuthData IO Response 
site location handlerPath acid = implSite location handlerPath (api acid)

site' :: SessionConf -> Text -> Text -> ACID -> ServerPartT IO Response
site' sessionConf location handlerPath acid = withClientSessionT sessionConf $ site location handlerPath acid
