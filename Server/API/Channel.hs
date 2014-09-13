{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Channel
where

import Prelude hiding ( id, (.) )
import Control.Category ( Category(id, (.)) )
import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
        ( Response, ok, method
        , Method (POST, GET, HEAD)
        , FilterMonad, look
        )
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative ((<$>))
import Control.Lens
import qualified Data.Set as S
import Data.Time.Clock
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes.Boomerang
import Control.Monad.Trans.JSON
import Text.Read (readMaybe)

import Model
import Model.Message hiding (id)
import ACID
import API.APIMonad
import API.Config
import API.Utils
import API.Errors
import API.Auth hiding (timestamp)
import API.ImageUtils

data API
    = Base
    | Messages 
    | Users
    deriving (Generic)

$(makeBoomerangs ''API)

channelroutes :: Router () (API :- ())
channelroutes =
    (  rBase 
    <> "messages" . rMessages
    <> "users" . rUsers
    )

route :: (Monad m, MonadIO m, Functor m)
      => ACID -> ChanId -> API -> APIMonadT API AuthData m Response
route acid cid url = case url of
    Base        ->      (method [GET, HEAD] >> getHandler acid cid)
                `mplus` (method [POST]      >> setHandler acid cid)
    Messages    ->      (method [GET, HEAD] >> getMessagesHandler acid cid)
                `mplus` (method [POST]      >> postHandler acid cid)
    Users       ->      (method [POST, HEAD]>> setUsersHandler acid cid)

genericHandler acid = (method [GET, HEAD] >> searchHandler acid)
              `mplus` (method POST >> createHandler acid)

searchHandler acid = ok' "Channel.searchHandler"

createHandler acid = handleError $ 
    updateWithJSONResponse acid $ do
        trySessionLoginU
        n <- prop "name"
        d <- prop "description"
        "id"    <$ createChannelU n d

getHandler acid cid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        "name"          <$ getChanNameQ cid
        "description"   <$ getChanDescQ cid      
        "type"          <$ getChanTypeQ cid
        "amountOfUsers" <$ amountOfDistinctUsersQ cid
        "lastPost"      <$ lastPostTimestampQ cid

setHandler acid cid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        "name"          ?> setChanNameU cid
        "description"   ?> setChanDescU cid  
        "type"          ?> setChanTypeU cid
        noContent'

getMessagesHandler acid cid = do
    o <- maybe 0 id . readMaybe <$> look "offset"
    a <- maybe 10 id . readMaybe <$> look "amount"
    handleError $ queryWithJSONResponse acid $ do
        trySessionLoginQ
        msgs <- messagesQ cid o a
        "messages" <$: flip fmap msgs .$ \ msg -> do
            "image"     <: view image msg
            "text"      <: view text msg
            "timestamp" <: view timestamp msg 
            let uid = view author msg  
            "author"    <$. userInfoQ uid

postHandler acid cid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        ts <- liftIO $ getCurrentTime
        t <- prop "text"
        img <- "image" .?> do
            typ <- prop "type"
            dat <- prop "data"
            cfg <- config imageConfig 
            storeImage cfg typ dat 
        postU cid ts t img
        noContent'   

setUsersHandler acid cid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        ts <- liftIO $ getCurrentTime
        oid <- getOperatorIdU
        let wN = withNotification ts oid
        "addOwners"         ?> sequence . fmap (wN $ addChanOwnerU cid)
        "removeOwners"      ?> sequence . fmap (wN $ rmChanOwnerU cid)
        "addProducers"      ?> sequence . fmap (wN $ addChanProducerU cid)
        "removeProducers"   ?> sequence . fmap (wN $ rmChanProducerU cid)
        "addConsumers"      ?> sequence . fmap (wN $ addChanConsumerU cid)
        "removeConsumers"   ?> sequence . fmap (wN $ rmChanConsumerU cid) 
        noContent'
    where
    withNotification ts oid f uid = f uid >> tryToAddUserNotificationU uid (AddedToChannel ts oid cid)
