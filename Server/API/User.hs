{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module API.User 
where

import Model
import Model.Permissions (hasAccess, forUserSelfOrAdmins)
import API.Effects
import API.Config
import API.Utils
import API.Auth hiding (timestamp)
import API.ImageUtils

import Prelude hiding ( id, (.) )
import Data.Text
import qualified Data.ByteString.Char8 as BS 
import qualified Data.Text as T
import Control.Category ( Category(id, (.)) )
import Web.Routes
import Control.Eff
import Control.Eff.JSON
import Data.Aeson (Value)
import qualified Data.Set as S
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes.Boomerang
import Text.Email.Validate (validate)

data UserAPI
    = Base 
    | Contacts
    | Subscriptions
    | Notifications -- when a user in my contact list added me to a channel
    deriving (Generic)

$(makeBoomerangs ''UserAPI)

instance PathInfo UserAPI

userroutes :: Router () (UserAPI :- ())
userroutes = 
    (  rBase 
    <> "contacts" . rContacts
    <> "subscriptions" . rSubscriptions
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
        Notifications -> case m of
            GET     -> getNotificationsHandler uid
            otherwise -> methodNotSupported

genericHandler = do
    m <- method
    case m of
        GET         -> searchHandler
        POST        -> createHandler
        otherwise   -> methodNotSupported

searchHandler = withJSONOut $ do
    trySessionLogin
    l' <- fmap (fmap T.pack) $ lookGet "login"
    -- TODO: implement minimal length of input here
    case l' of
        Just l -> do
            uids <- searchUserByLogin l 
            "result" <$: fmap userInfo (S.toList uids)
            return ()
        Nothing -> do
            "error" <: ("No query parameter given." :: String)
            return ()
    

createHandler = withJSONIO $ do
    trySessionLogin
    l <- makeLogin =<< prop "login"
    p <- makePassword =<< prop "password"
    "id"    <$. createUser l p

getHandler uid = withJSONOut $ do
    trySessionLogin
    userInfo uid
    sp <- hasAccess uid forUserSelfOrAdmins
    if sp
        then "email" <$ getUserEmail uid >> return ()
        else return ()

setHandler uid = withJSONIn $ do
    trySessionLogin 
    l <- "login"    ?> \l -> do
        l' <- makeLogin l
        setUserLogin uid l' 
        return l'
    p <- "password" ?> \ p -> do 
        p' <- makePassword p
        setUserPassword uid p' 
        return p'
    "name"          ?> \ n -> makeName n >>= setUserName uid
    "description"   ?> \ d -> makeDesc d >>= setUserDesc uid
    "email"         ?> \ e -> do
        case e of
            Nothing -> setUserEmail uid Nothing
            Just e' -> case validate . BS.pack $ e' of
                Left err -> throwJSONError $ CantDecodeProperty "email" (show err)
                Right e'' -> setUserEmail uid (Just e'') 
    "icon"          .?> do
        typ <- prop "type"
        dat <- prop "data"
        old <- getUserIcon uid
        icon <- storeUserIcon uid typ dat
        case icon of
            Left err -> throwJSONError $ CantDecodeProperty "icon" (show err)
            Right r -> setUserIcon uid (Just r)
    refreshCookie l $ p
    return Nothing

getContactsHandler uid = withJSONOut $ do
    trySessionLogin
    uids <- getUserContacts uid
    "contacts" <$: fmap userInfo (S.toList uids) 
    --"contacts" <$: flip fmap (S.toList uids) .$ \ uid -> do
    --    "login"         <$ getUserLogin uid
    --    "description"   <$ getUserDesc uid
    --    "icon"          <$ getUserIcon uid

setContactsHandler uid = withJSONIn $ do
    trySessionLogin
    "add"       ?> sequence . fmap (addUserContact uid)
    "remove"    ?> sequence . fmap (rmUserContact uid)
    return Nothing

getSubscriptionsHandler uid = withJSONOut $ do
   trySessionLogin
   cids <- getUserSubscriptions uid
   "subscriptions" <$: fmap channelInfo (S.toList cids)

setSubscriptionsHandler uid = withJSONIn $ do
    trySessionLogin
    "subscribe"     ?> sequence . fmap (subscribeToChan uid)
    "unsubscribe"   ?> sequence . fmap (unsubscribeFromChan uid)
    return Nothing

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
