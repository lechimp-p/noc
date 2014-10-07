{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module API
where

import Model
import Model.BaseTypes (UserId (..), ChanId (..))
import API.Effects
import API.Auth
import API.Config
import API.Session
import API.Utils
import API.User
import API.Channel 
import qualified API.User as User
import qualified API.Channel as Channel

import Prelude hiding ( id, (.) )
import Control.Category ( Category(id, (.)) )
import Data.Text (pack)
import Web.Routes
import Control.Eff
import Control.Eff.JSON
import Web.Routes.Boomerang
import Data.Aeson (Value (..))
import Text.Boomerang.TH (makeBoomerangs)
import Control.Lens

data RootAPI
    = Login 
    | Logout
    | User Int UserAPI
    | UserGeneric
    | Channel Int ChannelAPI
    | ChannelGeneric 
    | Default
    deriving (Generic)

$(makeBoomerangs ''RootAPI)

instance PathInfo RootAPI

apiroutes :: Router () (RootAPI :- ())
apiroutes = 
    (  "login" . rLogin
    <> "logout" . rLogout
    <> "user" . user
    <> "channel" . channel
    <> rDefault
    )
    where
    user =      rUserGeneric
             <> rUser </> int </> User.userroutes 
    channel =   rChannelGeneric 
             <> rChannel </> int </> Channel.channelroutes


route :: (Member API r, Member Exec r, Member Query r, Member Update r)
      => RootAPI -> Eff r (Either Error (Maybe Value))  
route url = do
    m <- method
    case url of
        Login -> case m of
            POST            -> logUserIn 
            otherwise       -> methodNotSupported 
        Logout -> case m of
            POST            -> logUserOut  
            otherwise       -> methodNotSupported 
        User uid uapi       -> User.route (UserId uid) uapi 
        UserGeneric         -> error "Route UserGeneric undefined."
        Channel cid capi    -> Channel.route (ChanId cid) capi
        ChannelGeneric      -> error "Route UserGeneric undefined."
        Default             -> helloWorld


helloWorld :: Member API r 
           => Eff r (Either Error (Maybe Value)) 
helloWorld = return . Right . Just . String =<< config _helloWorldMessage

