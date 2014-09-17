{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Update where

import Data.Text
import Data.Time.Clock
import Data.Data (Data, Typeable)

import Model.BaseTypes
--import Model.Channel
--import Model.User

data Update n
    = CreateChan Name (ChanId -> n)
    | CreateUser Login Password (UserId -> n) 
    | AddAdmin UserId (() -> n)
    | RmAdmin UserId (() -> n)
    | ChanUpdate ChanId (ChanUpdateType n)
    | UserUpdate UserId (UserUpdateType n)
    deriving (Typeable, Functor)

data ChanUpdateType n
    = SetChanName Name (() -> n) 
    | SetChanDesc Desc (() -> n)
    | SetChanType ChanType (() -> n) 
    | AddChanOwner UserId (() -> n) 
    | RmChanOwner UserId (() -> n) 
    | AddChanProducer UserId (() -> n) 
    | RmChanProducer UserId (() -> n) 
    | AddChanConsumer UserId (() -> n) 
    | RmChanConsumer UserId (() -> n) 
    | Post UTCTime Text (Maybe Image) (() -> n) 
    deriving (Typeable, Functor)

data UserUpdateType n
    = SetUserLogin Login (() -> n) 
    | SetUserPassword Password (() -> n) 
    | SetUserName Name (() -> n) 
    | SetUserDesc Desc (() -> n) 
    | SetUserIcon Icon (() -> n) 
    | AddUserNotification Notification (() -> n) 
    | RmUserNotification Notification (() -> n) 
    | AddUserContact UserId (() -> n) 
    | RmUserContact UserId (() -> n) 
    | AddUserSubscription UserId (() -> n) 
    | RmUserSubscription UserId (() -> n) 
    deriving (Typeable, Functor)


