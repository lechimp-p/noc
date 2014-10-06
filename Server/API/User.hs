{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module API.User 
where

import Model
import API.Effects
import API.Config
import API.Utils
import API.Auth hiding (timestamp)
import API.ImageUtils

import Prelude hiding ( id, (.) )
import Data.Text
import Control.Category ( Category(id, (.)) )
import Web.Routes
import Control.Eff
import Control.Eff.JSON
import Data.Aeson (Value)
import qualified Data.Set as S
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes.Boomerang

data UserAPI
    = Base 
    | Contacts
    | Subscriptions
    | Channels
    | Notifications -- when a user in my contact list added me to a channel
    deriving (Generic)

$(makeBoomerangs ''UserAPI)

instance PathInfo UserAPI

--userroutes :: Router () (API :- ())
userroutes = 
    (  rBase 
    <> "contacts" . rContacts
    <> "subscriptions" . rSubscriptions
    <> "channels" . rChannels
    <> "notifications" . rNotifications
    )

route :: (Member API r, Member Exec r, Member Query r, Member Update r)
      => UserId -> UserAPI -> Eff r (Either Error (Maybe Value))
route uid url = do
    m <- method
    case url of
        Base -> case m of
            GET     -> getHandler uid
            POST    -> setHandler uid
            otherwise -> methodNotSupported
        Contacts -> case m of
            GET     -> getContactsHandler uid
            POST    -> setContactsHandler uid
            otherwise -> methodNotSupported
        Subscriptions -> case m of
            GET     -> getSubscriptionsHandler uid
            POST    -> setSubscriptionsHandler uid
            otherwise -> methodNotSupported
        Channels -> case m of
            GET     -> getChannelsHandler uid
            otherwise -> methodNotSupported
        Notifications -> case m of
            GET     -> getNotificationsHandler uid
            otherwise -> methodNotSupported

searchHandler = error "User.searchHandler" 

createHandler = withJSONIO $ do
    trySessionLogin
    l <- prop "login"
    p <- prop "password"
    "id"    <$. createUser l p

getHandler uid = withJSONOut $ do
    trySessionLogin
    "login"         <$ getUserLogin uid
    "name"          <$ getUserName uid
    "description"   <$ getUserDesc uid
    "icon"          <$ getUserIcon uid

setHandler uid = withJSONIn $ do
    trySessionLogin 
    l <- "login"    ?> \ l -> setUserLogin uid l >> return l
    p <- "password" ?> \ p -> setUserPassword uid p >> return p
    "name"          ?> setUserName uid
    "description"   ?> setUserDesc uid
    "icon"          .?> do
        typ <- prop "type"
        dat <- prop "data"
        old <- getUserIcon uid
        icon <- storeIcon uid typ dat
        case icon of
            Left err -> throwJSONError $ CantDecodeProperty "icon" (show err)
            Right r -> setUserIcon uid (Just r)
    refreshCookie l $ p
    return Nothing

getContactsHandler uid = withJSONOut $ do
    trySessionLogin
    uids <- getUserContacts uid
    "contacts" <$: flip fmap (S.toList uids) .$ \ uid -> do
        "login"         <$ getUserLogin uid
        "description"   <$ getUserDesc uid
        "icon"          <$ getUserIcon uid

setContactsHandler uid = withJSONIn $ do
    trySessionLogin
    "add"       ?> sequence . fmap (addUserContact uid)
    "remove"    ?> sequence . fmap (rmUserContact uid)
    return Nothing

getSubscriptionsHandler uid = withJSONOut $ do
   trySessionLogin
   cids <- getUserSubscriptions uid
   showChannels cids

setSubscriptionsHandler uid = withJSONIn $ do
    trySessionLogin
    "subscribe"     ?> sequence . fmap (subscribeToChan uid)
    "unsubscribe"   ?> sequence . fmap (unsubscribeFromChan uid)
    return Nothing

getChannelsHandler uid = withJSONOut $ do
    trySessionLogin
    cids <- getUserSubscriptions uid
    showChannels cids

showChannels cids = do         
    "subscriptions" <$: flip fmap (S.toList cids) .$ \ cid -> do
        "name"          <$ getChanName cid
        "description"   <$ getChanDesc cid
        "type"          <$ getChanType cid

getNotificationsHandler uid = withJSONOut $ do
    trySessionLogin
    ns <- getUserNotifications uid
    "notifications" <$: flip fmap ns .$ \ n ->
        case n of
            AddedToChannel ts uid cid -> do
                "type" <: ("added-to-channel" :: Text)
                "timestamp" <: ts
                "user"      <$. userInfo uid
                "channel"   <$. channelInfo cid  
