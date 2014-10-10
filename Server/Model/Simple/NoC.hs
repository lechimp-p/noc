{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Simple.NoC
where

import Prelude hiding (words)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text 
import Data.Time.Clock (UTCTime)
import Data.IxSet (Indexable, empty, IxSet, ixSet, ixFun) 
import qualified Data.IxSet as IX
import Control.Lens (makeLenses)

import Model.BaseTypes
import Model.Simple.Channel
import Model.Simple.User
import Model.Simple.Message

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

makeLenses ''NoC

mkNoC :: Login -> Password -> NoC
mkNoC l pw = NoC IX.empty (ChanId 0)
                 users (UserId 1)
                 IX.empty (MsgId 0)
                 ((UserId 0) `S.insert` S.empty) 
    where
    users = admin `IX.insert` IX.empty 
    admin = User (UserId 0) l pw (Name "admin") (Desc "The Administrator.") Nothing Nothing S.empty S.empty S.empty []
