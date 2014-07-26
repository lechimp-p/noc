{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.BaseTypes
    ( UserId (..)
    , ChanId (..)
    , MsgId (..)
    , Login (..)
    , Name (..)
    , Desc (..)
    , Image (..)
    , Icon (..)
    )
where

import Data.Text
import Data.Data (Data, Typeable) 
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

newtype UserId = UserId { uiToInt :: Int } deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype ChanId = ChanId { ciToInt :: Int } deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype MsgId = MsgId { miToInt :: Int } deriving (Eq, Ord, Data, Typeable, SafeCopy)

newtype Login = Login { loginToText :: Text } deriving (Data, Typeable, SafeCopy)
newtype Name = Name { nameToText :: Text } deriving (Data, Typeable, SafeCopy)
newtype Desc = Desc { descToText :: Text } deriving (Data, Typeable, SafeCopy)

newtype Image = Image { imgPath :: String } deriving (Data, Typeable, SafeCopy)
newtype Icon = Icon { icnPath :: String } deriving (Data, Typeable, SafeCopy)
