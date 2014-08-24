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
import Control.Monad.Trans.JSON

import Model
import ACID
import API.APIMonad
import API.Utils
import API.Errors
import API.ImageUtils
import API.Auth 

data API
    = Base 
    | Contacts
    | Subscriptions
    | Channels
    | Notifications -- when a user in my contact list added me to a channel
    deriving (Generic)

$(makeBoomerangs ''API)

--userroutes :: Router () (API :- ())
userroutes = 
    (  rBase 
    <> "contacts" . rContacts
    <> "subscriptions" . rSubscriptions
    <> "channels" . rChannels
    <> "notifications" . rNotifications
    )

route :: (Monad m, MonadIO m, Functor m)
      => ACID -> UserId -> API -> APIMonadT API AuthData m Response
route acid uid url = case url of
    Base            ->      (method [GET, HEAD] >> getHandler acid uid)
                    `mplus` (method [POST]      >> setHandler acid uid)
    Contacts        ->      (method [GET, HEAD] >> getContactsHandler acid uid)
                    `mplus` (method [POST]      >> setContactsHandler acid uid)
    Subscriptions   ->      (method [GET, HEAD] >> getSubscriptionsHandler acid uid)
                    `mplus` (method [POST]      >> setSubscriptionsHandler acid uid)
    Channels        ->      (method [GET, HEAD] >> getChannelsHandler acid uid)
    Notifications   -> ok' "notifications"

genericHandler acid = (method [GET, HEAD] >> searchHandler acid)
              `mplus` (method POST >> createHandler acid)

searchHandler acid = ok' "User.searchHandler" 

createHandler acid = handleError $
    updateWithJSONResponse acid $ do
        trySessionLoginU
        l <- prop "login"
        p <- prop "password"
        "id"    <$. createUserU l p

getHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ 
        "login"         <$ getUserLoginQ uid
        "name"          <$ getUserNameQ uid
        "description"   <$ getUserDescQ uid
        "icon"          <$ getUserIconQ uid

setHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU 
        l <- "login"    ?> \ l -> setUserLoginU uid l >> return l
        p <- "password" ?> \ p -> setUserPasswordU uid p >> return p
        "name"          ?> setUserNameU uid
        "description"   ?> setUserDescU uid
        "icon"          .?> do
            typ <- prop "type"
            dat <- prop "data"
            old <- getUserIconU uid
            icon <- storeIcon defaultConfig uid typ dat
            catchError (setUserIconU uid (Just icon))
                       (\ _ -> do
                            removeIcon defaultConfig icon
                            setUserIconU uid old
                       )           
        refreshCookie l $ p
        noContent'

getContactsHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        uids <- getUserContactsQ uid
        "contacts" <$: flip fmap (S.toList uids) .$ \ uid -> do
            "login"         <$ getUserLoginQ uid
            "description"   <$ getUserDescQ uid
            "icon"          <$ getUserIconQ uid

setContactsHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        "add"       ?> sequence . fmap (addUserContactU uid)
        "remove"    ?> sequence . fmap (rmUserContactU uid)
        noContent'

getSubscriptionsHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        cids <- getUserSubscriptionsQ uid
        showChannels cids

setSubscriptionsHandler acid uid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        "subscribe"     ?> sequence . fmap (subscribeToChanU uid)
        "unsubscribe"   ?> sequence . fmap (unsubscribeFromChanU uid)
        noContent'

getChannelsHandler acid uid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        cids <- getUserOwnedChannelsQ uid
        showChannels cids

showChannels cids = do         
    "subscriptions" <$: flip fmap (S.toList cids) .$ \ cid -> do
        "name"          <$ getChanNameQ cid
        "description"   <$ getChanDescQ cid
        "type"          <$ getChanTypeQ cid

