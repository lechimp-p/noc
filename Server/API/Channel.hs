{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Channel
where

import Model
import Model.Message hiding (id)
import API.Effects
import API.Config
import API.Utils
import API.Auth hiding (timestamp)
import API.ImageUtils

import Prelude hiding ( id, (.) )
import Control.Category ( Category(id, (.)) )
import Web.Routes
import Control.Eff
import Control.Eff.JSON
import Data.Aeson (Value)
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes.Boomerang
import Text.Read (readMaybe)

data ChannelAPI
    = Base
    | Messages 
    | Users
    deriving (Generic)

$(makeBoomerangs ''ChannelAPI)

instance PathInfo ChannelAPI

channelroutes :: Router () (ChannelAPI :- ())
channelroutes =
    (  rBase 
    <> "messages" . rMessages
    <> "users" . rUsers
    )

route :: (Member API r, Member Exec r, Member Query r, Member Update r)
      => ChanId -> ChannelAPI -> Eff r (Either Error (Maybe Value))
route cid url = do
    m <- method 
    case url of
        Base -> case m of
            GET     -> getHandler cid
            POST    -> setHandler cid
            otherwise -> methodNotSupported 
        Messages -> case m of 
            GET         -> getMessagesHandler cid
            POST        -> postHandler cid
            otherwise   -> methodNotSupported
        Users -> case m of
            POST        -> setUsersHandler cid
            otherwise   -> methodNotSupported 

genericHandler = do
    m <- method
    case m of
        GET         -> searchHandler
        POST        -> createHandler
        otherwise   -> methodNotSupported

searchHandler = error "Channel.searchHandler"

createHandler = withJSONIO $ do 
    trySessionLogin
    n <- prop "name"
    d <- maybeProp "description"
    cid <- createChannel n
    ifIsJust d (setChanDesc cid)
    "id" <: cid 

getHandler cid = withJSONOut $ do
    trySessionLogin
    "name"          <$ getChanName cid
    "description"   <$ getChanDesc cid      
    "type"          <$ getChanType cid
    "amountOfUsers" <$ amountOfSubscribedUsers cid
    "lastPost"      <$ lastPostTimestamp cid

setHandler cid = withJSONIn $ do 
    trySessionLogin
    "name"          ?> setChanName cid
    "description"   ?> setChanDesc cid  
    "type"          ?> setChanType cid
    return Nothing

getMessagesHandler cid = withJSONOut $ do
    o <- fmap (maybe 0 id . readMaybe) $ lookGet "offset"
    a <- fmap (maybe 10 id . readMaybe) $ lookGet "amount"
    ts <- fmap readMaybe $ lookGet "timestamp"
    trySessionLogin
    msgs <- case ts of
        Nothing -> messages cid o a
        Just ts -> messagesTill cid ts
    "messages" <$: flip fmap msgs .$ \ msg -> do
        "image"     <: _image msg
        "text"      <: _text msg
        "timestamp" <: _timestamp msg 
        let uid = _author msg  
        "author"    <$. userInfo uid

postHandler cid = withJSONIn $ do 
    trySessionLogin
    ts <- timestamp 
    t <- prop "text"
    img <- "image" .??> do
        typ <- prop "type"
        dat <- prop "data"
        storeImage typ dat 
    oid <- forceOperatorId
    post cid oid ts t img
    return Nothing

setUsersHandler cid = withJSONIn $ do 
    trySessionLogin
    ts <- timestamp
    oid <- forceOperatorId
    let wN = withNotification ts oid
    "addOwners"         ?> sequence . fmap (wN $ addChanOwner cid)
    "removeOwners"      ?> sequence . fmap (wN $ rmChanOwner cid)
    "addProducers"      ?> sequence . fmap (wN $ addChanProducer cid)
    "removeProducers"   ?> sequence . fmap (wN $ rmChanProducer cid)
    "addConsumers"      ?> sequence . fmap (wN $ addChanConsumer cid)
    "removeConsumers"   ?> sequence . fmap (wN $ rmChanConsumer cid) 
    return Nothing
    where
    withNotification ts oid f uid = f uid >> tryToAddUserNotification uid (AddedToChannel ts oid cid)
