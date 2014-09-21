{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Message 
where

import Prelude hiding (words)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text 
import Data.Time.Clock (UTCTime)

import Model.BaseTypes

data Message = Message 
    { _id           :: MsgId
    , _chan         :: ChanId
    , _image        :: Maybe Image
    , _text         :: Text
    , _author       :: UserId
    , _timestamp    :: UTCTime
    }
    deriving (Data, Typeable)

instance Eq Message where
    m == m' = _id m == _id m' 

instance Ord Message where
    compare m m' = compare (_id m) (_id m')

