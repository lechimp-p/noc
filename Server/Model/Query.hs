{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Query where

import Data.Text
import Data.Time.Clock
import Data.Data (Data, Typeable)

import Model.BaseTypes
import Model.Message
--import Model.Channel
--import Model.User


data Query n
    = IsAdmin UserId (Bool -> n)
    | ChanQuery ChanId (ChanQueryType n) 
    | UserQuery UserId (UserQueryType n)
    deriving (Typeable, Functor)

{--instance Functor Query where
    fmap f (IsAdmin uid n) = IsAdmin uid (fmap f n)
    fmap f (GetOperatorId n) = GetOperatorId (fmap f n)
    fmap f (ChanQuery cid n) = ChanQuery cid (fmap f n)
    fmap f (UserQuery uid n) = UserQuery uid (fmap f n)
--}

type Offset = Int
type Amount = Int

data ChanQueryType n
    = GetChanName               (Name -> n)
    | GetChanDesc               (Desc -> n)
    | GetChanType               (ChanType -> n)
    | GetChanImage              (Maybe Image -> n)
    | IsChanOwner UserId        (Bool -> n)
    | IsChanProducer UserId     (Bool -> n)
    | IsChanConsumer UserId     (Bool -> n)
    | AmountOfDistinctUsers     (Int -> n)
    | LastPostTimestamp         (UTCTime -> n)
    | Messages Offset Amount    ([Message] -> n) 
    | MessagesTill UTCTime      ([Message] -> n)
    deriving (Typeable, Functor)

{--instance Functor ChanQueryType where
    fmap f (GetChanName n) = GetChanName (fmap f n) 
    fmap f (GetChanDesc n) = GetChanDesc (fmap f n)
    fmap f (GetChanType n) = GetChanType (fmap f n)
    fmap f (IsChanOwner uid n) = IsChanOwner uid (fmap f n)
    fmap f (IsChanProducer uid n) = IsChanProducer uid (fmap f n)
    fmap f (IsChanConsumer uid n) = IsChanConsumer uid (fmap f n)
    fmap f (AmountOfDistinctUsers n) = AmountOfDistinctUsers (fmap f n)
    fmap f (LastPostTimestamp n) = LastPostTimestamp (fmap f n)
    fmap f (Messages o a n) = Messages o a (fmap f n)
    fmap f (MessagesTill ts n) = MessagesTill ts (fmap f n)
--}

 
data UserQueryType n
    = GetUserLogin (Login -> n)
    | IsUserPassword Password (Bool -> n)
    | GetUserName (Name -> n)
    | GetUserDesc (Desc -> n)
    | GetUserIcon (Icon -> n)
    | GetUserNotifications ([Notification] -> n)
    | GetUserContacts ([UserId] -> n)
    | GetUserSubscriptions ([ChanId] -> n)
    deriving (Typeable, Functor)
