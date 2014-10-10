{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.BaseTypes
    ( Offset
    , Amount
    , UserId (..)
    , ChanId (..)
    , MsgId (..)
    , Login (..)
    , Password (..)
    , Name (..)
    , Desc (..)
    , Image (..)
    , Icon (..)
    , checkPassword
    , ChanType (..)
    , Notification (..)
    )
where

import Data.Text
import Data.Time.Clock
import Data.Data (Data, Typeable) 

type Offset = Int
type Amount = Int

newtype UserId = UserId { uiToInt :: Int } deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype ChanId = ChanId { ciToInt :: Int } deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype MsgId = MsgId { miToInt :: Int } deriving (Show, Read, Eq, Ord, Data, Typeable)

newtype Login = Login { loginToText :: Text } deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype Password = Password { pwToText :: Text } deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype Name = Name { nameToText :: Text } deriving (Data, Typeable)
newtype Desc = Desc { descToText :: Text } deriving (Data, Typeable)

newtype Image = Image { imgPath :: Text } deriving (Data, Typeable)
newtype Icon = Icon { icnPath :: Text } deriving (Data, Typeable)

checkPassword :: Password -> Password -> Bool
checkPassword pw pw' = pwToText pw == pwToText pw'

data ChanType
    = None
    | Stream
    | Conversation
    deriving (Data, Typeable)

data Notification
    = AddedToChannel UTCTime UserId ChanId
    deriving (Eq, Data, Typeable)
