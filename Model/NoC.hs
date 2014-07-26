{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.NoC
where

import Prelude hiding (words)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text 
import Data.Time.Clock (UTCTime)
import Data.IxSet (Indexable, empty, IxSet, ixSet, ixFun) 
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

import Model.BaseTypes
import Model.Channel
import Model.User
import Model.Message

data NoC = NoC 
    { _channels :: IxSet Channel
    , _user     :: IxSet User 
    , _messages :: IxSet Message
    , _admins   :: S.Set UserId
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''NoC)
