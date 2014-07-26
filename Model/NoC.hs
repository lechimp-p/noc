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
import Control.Lens (makeLenses)

import Model.BaseTypes
import Model.Channel
import Model.User
import Model.Message

data NoC = NoC 
    { _channels   :: IxSet Channel
    , _nextChanId :: ChanId 
    , _users      :: IxSet User 
    , _nextUserId :: UserId 
    , _messages   :: IxSet Message
    , _nextMsgId  :: MsgId 
    , _admins     :: S.Set UserId
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''NoC)
makeLenses ''NoC

