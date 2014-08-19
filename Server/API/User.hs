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
import Control.Monad.Trans.Class
import Control.Monad
import qualified Data.Set as S

import qualified Model.BaseTypes as BT
import ACID
import API.APIMonad
import API.Utils
import API.Errors
import API.JSONUtils
import API.Auth 

data API
    = Get
    | Set
    | UploadIcon
    | Contacts
    | Subscriptions
    | Channels
    | Notifications -- when a user in my contact list added me to a channel
    deriving (Generic)

route :: (Monad m, MonadIO m, Functor m)
      => ACID -> BT.UserId -> API -> APIMonadT API AuthData m Response
route acid uid url = case url of
    Get             -> method [GET, HEAD] >> getHandler acid uid 
    Set             -> method [POST, HEAD] >> setHandler acid uid
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> ok' "contacts\n"
    Subscriptions   -> ok' "subscriptions\n"
    Channels        -> ok' "channels\n"

getHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ 
        "login"         <:. getUserLoginQ uid
        "name"          <:. getUserNameQ uid
        "description"   <:. getUserDescQ uid

setHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU 
        l <- "login"    ?:> \ l -> setUserLoginU uid l >> return l
        p <- "password" ?:> \ p -> setUserPasswordU uid p >> return p
        "name"          ?:> setUserNameU uid
        "description"   ?:> setUserDescU uid
        refreshCookie l $ p
        noContent'

contactsHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        lift $ trySessionLoginQ
        uids <- lift $ getUserContactsQ uid
        "contacts" <:: flip fmap (S.toList uids) .$ \ uid -> do
            "login"         <:. fmap BT.loginToText .$ getUserLoginQ uid
            "description"   <:. fmap BT.descToText .$ getUserDescQ uid
            "icon"          <:. fmap (fmap BT.icnPath) .$ getUserIconQ uid
