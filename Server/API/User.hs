{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module API.User 
where

import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
        ( Response, ok, method
        , Method (POST, GET, HEAD)
        , FilterMonad
        )
--import Data.Aeson
--import Data.Aeson.Types
--import Control.Applicative
import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import Control.Monad
import qualified Data.Set as S

import Model
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
    | AddToContacts
    | RemoveFromContacts
    deriving (Generic)

route :: (Monad m, MonadIO m, Functor m)
      => ACID -> UserId -> API -> APIMonadT API AuthData m Response
route acid uid url = case url of
    Get             -> method [GET, HEAD]   >> getHandler acid uid 
    Set             -> method [POST, HEAD]  >> setHandler acid uid
    UploadIcon      -> ok' "uploadIcon\n"
    Contacts        -> method [GET, HEAD]   >> contactsHandler acid uid
    Subscriptions   -> method [GET, HEAD]   >> subscriptionsHandler acid uid
    Channels        -> method [GET, HEAD]   >> channelsHandler acid uid
    Notifications   -> ok' "notifications"
    AddToContacts       -> method [POST, HEAD]  >> addToContactsHandler acid uid
    RemoveFromContacts  -> method [POST, HEAD]  >> removeFromContactsHandler acid uid

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
        trySessionLoginQ
        uids <- getUserContactsQ uid
        "contacts" <:: flip fmap (S.toList uids) .$ \ uid -> do
            "login"         <:. getUserLoginQ uid
            "description"   <:. getUserDescQ uid
            "icon"          <:. getUserIconQ uid

subscriptionsHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        cids <- getUserSubscriptionsQ uid
        showChannels cids

channelsHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        cids <- getUserOwnedChannelsQ uid
        showChannels cids

showChannels cids = do         
    "subscriptions" <:: flip fmap (S.toList cids) .$ \ cid -> do
        "name"          <:. getChanNameQ cid
        "description"   <:. getChanDescQ cid

addToContactsHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        oid <- getOperatorIdU
        addUserContactU oid uid
        noContent'

removeFromContactsHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        oid <- getOperatorIdU
        rmUserContactU oid uid
        noContent'
