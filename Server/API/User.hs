{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module API.User 
where

import Prelude hiding ( id, (.) )
import Control.Category ( Category(id, (.)) )
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
        ( Response, ok, method
        , Method (POST, GET, HEAD)
        , FilterMonad
        )
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad
import qualified Data.Set as S
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes.Boomerang

import Model
import ACID
import API.APIMonad
import API.Utils
import API.Errors
import API.JSONUtils
import API.ImageUtils
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

$(makeBoomerangs ''API)

--userroutes :: Router () (API :- ())
userroutes = 
    (  "get" . rGet
    <> "set" . rSet
    <> "upload-icon" . rUploadIcon
    <> "contacts" . rContacts
    <> "subscriptions" . rSubscriptions
    <> "channels" . rChannels
    <> "notifications" . rNotifications
    <> "add-to-contacts" . rAddToContacts
    <> "remove-from-contacts" . rRemoveFromContacts
    )

route :: (Monad m, MonadIO m, Functor m)
      => ACID -> UserId -> API -> APIMonadT API AuthData m Response
route acid uid url = case url of
    Get             -> method [GET, HEAD]   >> getHandler acid uid 
    Set             -> method [POST, HEAD]  >> setHandler acid uid
    UploadIcon      -> method [POST, HEAD]  >> uploadIconHandler acid uid 
    Contacts        -> method [GET, HEAD]   >> contactsHandler acid uid
    Subscriptions   -> method [GET, HEAD]   >> subscriptionsHandler acid uid
    Channels        -> method [GET, HEAD]   >> channelsHandler acid uid
    Notifications   -> ok' "notifications"
    AddToContacts       -> method [POST, HEAD]  >> addToContactsHandler acid uid
    RemoveFromContacts  -> method [POST, HEAD]  >> removeFromContactsHandler acid uid

genericHandler acid = (method [GET, HEAD] >> searchHandler acid)
              `mplus` (method [POST] >> createHandler acid)

searchHandler acid = ok' "User.searchHandler" 

createHandler acid = handleError $
    updateWithJSONResponse acid $ do
        trySessionLoginU
        l <- prop "login"
        p <- prop "password"
        "id"    <:. createUserU l p

getHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ 
        "login"         <:. getUserLoginQ uid
        "name"          <:. getUserNameQ uid
        "description"   <:. getUserDescQ uid
        "icon"          <:. getUserIconQ uid

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
        "type"          <:. getChanTypeQ cid

addToContactsHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        oid <- getOperatorIdU
        addUserContactU oid uid
        noContent'

removeFromContactsHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        oid <- getOperatorIdU
        rmUserContactU oid uid
        noContent'

uploadIconHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        typ <- prop "type"
        dat <- prop "data"
        old <- getUserIconU uid
        icon <- storeIcon defaultConfig uid typ dat
        catchError (setUserIconU uid (Just icon))
                   (\ _ -> do
                        removeIcon defaultConfig icon
                        setUserIconU uid old
                   )
        noContent'
