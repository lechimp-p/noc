{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.BaseTypes
    ( UserId (..)
    , ChanId (..)
    , MsgId (..)
    , Login (..)
    , mkLogin
    , Password (..)
    , mkPassword
    , Name (..)
    , mkName
    , Desc (..)
    , mkDesc
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
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

newtype UserId = UserId { uiToInt :: Int } deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)
newtype ChanId = ChanId { ciToInt :: Int } deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)
newtype MsgId = MsgId { miToInt :: Int } deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)

newtype Login = Login { loginToText :: Text } deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)
mkLogin = Login
newtype Password = Password { pwToText :: Text } deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)
mkPassword = Password
newtype Name = Name { nameToText :: Text } deriving (Data, Typeable, SafeCopy)
mkName = Name
newtype Desc = Desc { descToText :: Text } deriving (Data, Typeable, SafeCopy)
mkDesc = Desc

newtype Image = Image { imgPath :: Text } deriving (Data, Typeable, SafeCopy)
newtype Icon = Icon { icnPath :: Text } deriving (Data, Typeable, SafeCopy)

checkPassword :: Password -> Password -> Bool
checkPassword pw pw' = pwToText pw == pwToText pw'

data ChanType
    = None
    | Stream
    | Conversation
    deriving (Data, Typeable)

data Notification
    = AddedToChannel UTCTime UserId ChanId
    deriving (Data, Typeable)
