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

setHandler acid uid = error "setHandler" 
{--
setHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        lift $ trySessionLoginU 
        l <- maybeProp "login"
        p <- maybeProp "password" 
        n <- maybeProp "name"
        d <- maybeProp "description"
        ifIsJust l $ lift . setUserLoginU uid
        ifIsJust p $ lift . setUserPasswordU uid
        ifIsJust n $ lift . setUserNameU uid
        ifIsJust d $ lift . setUserDescU uid
        lift . lift . refreshCookie l $ p
        lift . lift $ noContent'
--}
contactsHandler acid uid = error "contactsHandler" 
{--
contactsHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        lift $ trySessionLoginQ
        uids <- lift $ getUserContactsQ uid
        infos <- lift . flip mapM (S.toList uids) $ \ uid -> do
            l <- getUserLoginQ uid
            d <- getUserDescQ uid
            i <- getUserIconQ uid
            return . object $ [ "login"         .= BT.loginToText l
                              , "description"   .= BT.descToText d
                              , "icon"          .= fmap BT.icnPath i
                              ]
        "contacts" <: infos
--}
