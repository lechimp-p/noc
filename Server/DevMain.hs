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
import API (api)
import API.Config
import API.Utils (ACID)
import API.Auth (AuthData)
import API.APIMonad (InnerAPIMonadT)
import ACID.Query
import ACID.Update

initialNoC = mkNoC (mkLogin "admin") (mkPassword "admin") 
config' = Config 
    ( "Hello World!" )
    ( SessionConfig 
        "NoCSession"    
        Session         -- | MaxAge seconds | Expires UTCTime | Expired
        ""  
        "/"
        "client_session_key.aes"
        False
    )
    ( SiteConfig
        "http://localhost:8000"
        ""
    )
    ( "./state" )
    ( ImageConfig
        "./files"
        "user"
        "channel"
        0
    )
    ( BodyPolicy
        "/tmp/NoC-Server-dev"
        100000
        100000
        100000
    )    

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
site location handlerPath acid = implSite location handlerPath (api config' acid)

site' :: SessionConf -> Text -> Text -> ACID -> ServerPartT IO Response
site' sessionConf location handlerPath acid = withClientSessionT sessionConf $ site location handlerPath acid
