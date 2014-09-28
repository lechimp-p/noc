{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Model.Update where

import Data.Text
import Data.Time.Clock
import Data.Data (Typeable)
import Control.Eff

import Model.BaseTypes

data Update n
    = CreateChan UserId Name            (ChanId -> n)
    | CreateUser Login Password         (UserId -> n) 
    | AddAdmin UserId                   (() -> n)
    | RmAdmin UserId                    (() -> n)
    | ChanUpdate ChanId (ChanUpdateType n)
    | UserUpdate UserId (UserUpdateType n)
    deriving (Typeable, Functor)

createChan :: Member Update r => UserId -> Name -> Eff r ChanId
createChan uid n = send $ \ next -> inj (CreateChan uid n next)

createUser :: Member Update r => Login -> Password -> Eff r UserId
createUser l pw = send $ \ next -> inj (CreateUser l pw next)

addAdmin :: Member Update r => UserId -> Eff r ()
addAdmin uid = send $ \ next -> inj (AddAdmin uid next)

rmAdmin :: Member Update r => UserId -> Eff r ()
rmAdmin uid = send $ \ next -> inj (RmAdmin uid next)

data ChanUpdateType n
    = SetChanName Name                          (() -> n) 
    | SetChanDesc Desc                          (() -> n)
    | SetChanType ChanType                      (() -> n) 
    | SetChanImage (Maybe Image)                (() -> n)
    | AddChanOwner UserId                       (() -> n) 
    | RmChanOwner UserId                        (() -> n) 
    | AddChanProducer UserId                    (() -> n) 
    | RmChanProducer UserId                     (() -> n) 
    | AddChanConsumer UserId                    (() -> n) 
    | RmChanConsumer UserId                     (() -> n) 
    | Post UserId UTCTime Text (Maybe Image)    (MsgId -> n) 
    deriving (Typeable, Functor)

chanUpdate :: Member Update r 
          => ChanId -> (forall w. ((n -> VE r w) -> ChanUpdateType(VE r w))) -> Eff r n
chanUpdate cid q = send $ \ next -> inj (ChanUpdate cid (q next))

setChanName :: Member Update r => ChanId -> Name -> Eff r ()
setChanName cid n = chanUpdate cid (SetChanName n)

setChanDesc :: Member Update r => ChanId -> Desc -> Eff r ()
setChanDesc cid n = chanUpdate cid (SetChanDesc n)

setChanType :: Member Update r => ChanId -> ChanType -> Eff r ()
setChanType cid n = chanUpdate cid (SetChanType n)

setChanImage :: Member Update r => ChanId -> Maybe Image -> Eff r ()
setChanImage cid img = chanUpdate cid (SetChanImage img)

addChanOwner :: Member Update r => ChanId -> UserId -> Eff r ()
addChanOwner cid uid = chanUpdate cid (AddChanOwner uid)

rmChanOwner :: Member Update r => ChanId -> UserId -> Eff r ()
rmChanOwner cid uid = chanUpdate cid (RmChanOwner uid)

addChanProducer :: Member Update r => ChanId -> UserId -> Eff r ()
addChanProducer cid uid = chanUpdate cid (AddChanProducer uid)

rmChanProducer :: Member Update r => ChanId -> UserId -> Eff r ()
rmChanProducer cid uid = chanUpdate cid (RmChanProducer uid)

addChanConsumer :: Member Update r => ChanId -> UserId -> Eff r ()
addChanConsumer cid uid = chanUpdate cid (AddChanConsumer uid)

rmChanConsumer :: Member Update r => ChanId -> UserId -> Eff r ()
rmChanConsumer cid uid = chanUpdate cid (RmChanConsumer uid)

post :: Member Update r => ChanId -> UserId -> UTCTime -> Text -> Maybe Image -> Eff r MsgId 
post cid uid ts t img = chanUpdate cid (Post uid ts t img)

data UserUpdateType n
    = SetUserLogin Login                (() -> n) 
    | SetUserPassword Password          (() -> n) 
    | SetUserName Name                  (() -> n) 
    | SetUserDesc Desc                  (() -> n) 
    | SetUserIcon (Maybe Icon)          (() -> n) 
    | AddUserNotification Notification  (() -> n) 
    | AddUserContact UserId             (() -> n) 
    | RmUserContact UserId              (() -> n) 
    | AddUserSubscription ChanId        (() -> n) 
    | RmUserSubscription ChanId         (() -> n) 
    deriving (Typeable, Functor)

userUpdate :: Member Update r 
          => UserId -> (forall w. ((n -> VE r w) -> UserUpdateType(VE r w))) -> Eff r n
userUpdate uid q = send $ \ next -> inj (UserUpdate uid (q next))

setUserLogin :: Member Update r => UserId -> Login -> Eff r ()
setUserLogin uid l = userUpdate uid (SetUserLogin l)

setUserPassword :: Member Update r => UserId -> Password -> Eff r ()
setUserPassword uid l = userUpdate uid (SetUserPassword l)

setUserName :: Member Update r => UserId -> Name -> Eff r ()
setUserName uid l = userUpdate uid (SetUserName l)

setUserDesc :: Member Update r => UserId -> Desc -> Eff r ()
setUserDesc uid l = userUpdate uid (SetUserDesc l)

setUserIcon :: Member Update r => UserId -> (Maybe Icon) -> Eff r ()
setUserIcon uid l = userUpdate uid (SetUserIcon l)

addUserNotification :: Member Update r => UserId -> Notification -> Eff r ()
addUserNotification uid n = userUpdate uid (AddUserNotification n)

addUserContact :: Member Update r => UserId -> UserId -> Eff r ()
addUserContact uid o = userUpdate uid (AddUserContact o)

rmUserContact :: Member Update r => UserId -> UserId -> Eff r ()
rmUserContact uid o = userUpdate uid (RmUserContact o)

addUserSubscription :: Member Update r => UserId -> ChanId -> Eff r ()
addUserSubscription uid cid = userUpdate uid (AddUserSubscription cid)

rmUserSubscription :: Member Update r => UserId -> ChanId -> Eff r ()
rmUserSubscription uid cid = userUpdate uid (RmUserSubscription cid)
