{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Channel
where

import Web.Routes
import Web.Routes.Happstack
import Happstack.Server 
        ( Response, ok, method
        , Method (POST, GET, HEAD)
        , FilterMonad
        )
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens
import qualified Data.Set as S
import Data.Time.Clock

import Model
import Model.Message
import ACID
import API.APIMonad
import API.Utils
import API.Errors
import API.JSONUtils
import API.Auth hiding (timestamp)

data API
    = Get   -- name, desc, number of members, time of last post
    | Set
    | Messages -- name of author, timestamp, text, image
    | Post -- text, image
    | Subscribe
    | Unsubscribe
    deriving (Generic)

route :: (Monad m, MonadIO m, Functor m)
      => ACID -> ChanId -> API -> APIMonadT API AuthData m Response
route acid cid url = case url of
    Get         -> method [GET, HEAD]   >> getHandler acid cid
    Set         -> method [POST, HEAD]  >> setHandler acid cid
    Messages    -> method [GET, HEAD]   >> messagesHandler acid cid
    Post        -> method [POST, HEAD]  >> postHandler acid cid
    Subscribe   -> method [POST, HEAD]  >> subscribeHandler acid cid
    Unsubscribe -> method [POST, HEAD]  >> unsubscribeHandler acid cid

getHandler acid cid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        "name"          <:. getChanNameQ cid
        "description"   <:. getChanDescQ cid      

setHandler acid cid = handleError $
    updateWithJSONInput acid $ do
        trySessionLoginU
        "name"          ?:> setChanNameU cid
        "description"   ?:> setChanDescU cid  
        noContent'

messagesHandler acid cid = handleError $
    queryWithJSONResponse acid $ do
        trySessionLoginQ
        a <- prop "amount"
        o <- prop "offset"
        msgs <- messagesQ cid a o
        "messages" <:: flip fmap msgs .$ \ msg -> do
            "image"     <: view image msg
            "text"      <: view text msg
            "timestamp" <: view timestamp msg 
            "author"    <:.. do
                let uid = view author msg  
                "id"        <: uid
                "login"     <:. getUserLoginQ uid
                "icon"      <:. getUserIconQ uid

postHandler acid cid = handleError $
    updateWithJSONResponse acid $ do
        trySessionLoginU
        ts <- liftIO $ getCurrentTime
        t <- prop "text"
        -- ToDo: insert image here somehow
        "id" <:. postU cid ts t Nothing
 
subscribeHandler = error "subscribeHandler"
unsubscribeHandler = error "unsubscribeHandler"

