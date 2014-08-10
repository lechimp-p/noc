{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module API.User 
where

import Data.Text (pack, Text)
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
        ( Response, ok, method
        , Method (POST, GET, HEAD)
        , FilterMonad
        )
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad.IO.Class

import qualified Model.BaseTypes as BT
import API.APIMonad
import API.Utils
import API.Auth (AuthData, runHandler, refreshCookie)

data API
    = Get
    | Set
    | UploadIcon
    | Contacts
    | Subscriptions
    | Channels
    | Notifications -- when a user in my contact list added me to a channel
    deriving (Generic)

route :: (Monad m, MonadIO m)
      => ACID -> BT.UserId -> API -> APIMonadT API AuthData m Response
route acid uid url = case url of
    Get             -> method [GET, HEAD] >> getHandler acid uid 
    Set             -> method [POST, HEAD] >> setHandler acid uid
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> ok' "contacts\n"
    Subscriptions   -> ok' "subscriptions\n"
    Channels        -> ok' "channels\n"

getHandler acid uid = error "TODO" 
{--    let ta = \l p -> QueryTA $ GetUserTA uid l p
    in runHandler acid ta $ \ (l, n, d) -> jsonR' $ 
        object $ [ "login"          .= BT.loginToText l
                 , "name"           .= BT.nameToText n 
                 , "description"    .= BT.descToText d 
                 ]   
--}
setHandler acid uid = error "TODO"
    {--parseBody $ \ obj -> do
    l <- fmap (fmap BT.mkLogin)     $ obj .:? "login"
    p <- fmap (fmap BT.mkPassword)  $ obj .:? "password"
    n <- fmap (fmap BT.mkName)      $ obj .:? "name"
    d <- fmap (fmap BT.mkDesc)      $ obj .:? "description"
    return $ let ta = \l' p' -> UpdateTA $ SetUserTA l p n d uid l' p'
             in runHandler acid ta $ \ _ -> do
                    refreshCookie l p
                    noContent'  
    --}

