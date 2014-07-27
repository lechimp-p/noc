{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.NoC
where

import Prelude hiding (words)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text 
import Data.Time.Clock (UTCTime)
import Data.IxSet (Indexable, empty, IxSet, ixSet, ixFun) 
import qualified Data.IxSet as IX
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

mkNoC :: Login -> Password -> NoC
mkNoC l pw = NoC IX.empty (ChanId 0)
                 users (UserId 1)
                 IX.empty (MsgId 0)
                 ((UserId 0) `S.insert` S.empty) 
    where
    users = admin `IX.insert` IX.empty 
    admin = User (UserId 0) l pw (Name "admin") (Desc "The Administrator.") Nothing S.empty S.empty S.empty  
