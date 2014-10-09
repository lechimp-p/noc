{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Model.Query where

import Data.Text
import Data.Time.Clock
import Data.Data (Typeable)
import qualified Data.Set as S
import Control.Eff


import Model.BaseTypes
import Model.Message

data Query n
    = IsAdmin UserId (Bool -> n)
    | CountAdmins (Int -> n)
    | GetUserIdByLogin Text (Maybe UserId -> n)
    | SearchUserByLogin Text (S.Set UserId -> n)
    | SearchChanByName Text (S.Set ChanId -> n)
    | ChanQuery ChanId (ChanQueryType n) 
    | UserQuery UserId (UserQueryType n)
    deriving (Typeable, Functor)

isAdmin :: Member Query r => UserId -> Eff r Bool
isAdmin uid = send $ \ next -> inj (IsAdmin uid next)

countAdmins :: Member Query r => Eff r Int
countAdmins = send $ \ next -> inj (CountAdmins next)

getUserIdByLogin :: Member Query r => Text -> Eff r (Maybe UserId)
getUserIdByLogin l = send $ \ next -> inj (GetUserIdByLogin l next)

searchUserByLogin :: Member Query r => Text -> Eff r (S.Set UserId)
searchUserByLogin l = send $ \ next -> inj (SearchUserByLogin l next)

searchChanByName :: Member Query r => Text -> Eff r (S.Set ChanId)
searchChanByName n = send $ \ next -> inj (SearchChanByName n next)

data ChanQueryType n
    = GetChanName               (Name -> n)
    | GetChanDesc               (Desc -> n)
    | GetChanType               (ChanType -> n)
    | GetChanImage              (Maybe Image -> n)
    | IsChanOwner UserId        (Bool -> n)
    | IsChanProducer UserId     (Bool -> n)
    | IsChanConsumer UserId     (Bool -> n)
    | AmountOfSubscribedUsers   (Int -> n)
    | LastPostTimestamp         (Maybe UTCTime -> n)
    | Messages Offset Amount    ([Message] -> n) 
    | MessagesTill UTCTime      ([Message] -> n)
    deriving (Typeable, Functor)

chanQuery :: Member Query r 
          => ChanId -> (forall w. ((n -> VE r w) -> ChanQueryType (VE r w))) -> Eff r n
chanQuery cid q = send $ \ next -> inj (ChanQuery cid (q next))

getChanName :: Member Query r => ChanId -> Eff r Name
getChanName cid = chanQuery cid GetChanName 

getChanDesc :: Member Query r => ChanId -> Eff r Desc 
getChanDesc cid = chanQuery cid GetChanDesc 

getChanType :: Member Query r => ChanId -> Eff r ChanType 
getChanType cid = chanQuery cid GetChanType

getChanImage :: Member Query r => ChanId -> Eff r (Maybe Image)
getChanImage cid = chanQuery cid GetChanImage

isChanOwner :: Member Query r => ChanId -> UserId -> Eff r Bool
isChanOwner cid uid = chanQuery cid (IsChanOwner uid)

isChanProducer :: Member Query r => ChanId -> UserId -> Eff r Bool
isChanProducer cid uid = chanQuery cid (IsChanProducer uid)

isChanConsumer :: Member Query r => ChanId -> UserId -> Eff r Bool
isChanConsumer cid uid = chanQuery cid (IsChanConsumer uid)

amountOfSubscribedUsers :: Member Query r => ChanId -> Eff r Int
amountOfSubscribedUsers cid = chanQuery cid AmountOfSubscribedUsers

lastPostTimestamp :: Member Query r => ChanId -> Eff r (Maybe UTCTime)
lastPostTimestamp cid = chanQuery cid LastPostTimestamp

messages :: Member Query r => ChanId -> Offset -> Amount -> Eff r [Message]
messages cid o a = chanQuery cid (Messages o a) 

messagesTill :: Member Query r => ChanId -> UTCTime -> Eff r [Message]
messagesTill cid ts = chanQuery cid (MessagesTill ts)


data UserQueryType n
    = GetUserLogin              (Login -> n)
--    | IsUserPassword Password   (Bool -> n)
    | GetUserName               (Name -> n)
    | GetUserDesc               (Desc -> n)
    | GetUserIcon               (Maybe Icon -> n)
    | GetUserNotifications      ([Notification] -> n)
    | GetUserContacts           (S.Set UserId -> n)
    | GetUserSubscriptions      (S.Set ChanId -> n)
    deriving (Typeable, Functor)

userQuery :: Member Query r 
          => UserId -> (forall w. ((n -> VE r w) -> UserQueryType (VE r w))) -> Eff r n
userQuery uid q = send $ \ next -> inj (UserQuery uid (q next))

getUserLogin :: Member Query r => UserId -> Eff r Login
getUserLogin uid = userQuery uid GetUserLogin

--isUserPassword :: Member Query r => UserId -> Password -> Eff r Bool
--isUserPassword uid pw = userQuery uid (IsUserPassword pw)

getUserName :: Member Query r => UserId -> Eff r Name 
getUserName uid = userQuery uid GetUserName

getUserDesc :: Member Query r => UserId -> Eff r Desc 
getUserDesc uid = userQuery uid GetUserDesc

getUserIcon :: Member Query r => UserId -> Eff r (Maybe Icon) 
getUserIcon uid = userQuery uid GetUserIcon

getUserNotifications :: Member Query r => UserId -> Eff r [Notification] 
getUserNotifications uid = userQuery uid GetUserNotifications

getUserContacts :: Member Query r => UserId -> Eff r (S.Set UserId)
getUserContacts uid = userQuery uid GetUserContacts
 
getUserSubscriptions :: Member Query r => UserId -> Eff r (S.Set ChanId)
getUserSubscriptions uid = userQuery uid GetUserSubscriptions
