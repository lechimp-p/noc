{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Acid.SafeCopy
    ( module Model.Acid.SafeCopy
    , module Model.Simple.NoC
    )
where

import Model.BaseTypes
import Model.Errors
import Model.Simple.Channel
import Model.Simple.User
import Model.Simple.Message
import Model.Simple.NoC

import Data.SafeCopy
import Text.Email.Validate
import qualified Data.Set as S

$(deriveSafeCopy 0 'base ''EmailAddress)
$(deriveSafeCopy 0 'base ''PermissionViolation)
$(deriveSafeCopy 0 'base ''Error)
$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''ChanId)
$(deriveSafeCopy 0 'base ''MsgId)
$(deriveSafeCopy 0 'base ''Login)
$(deriveSafeCopy 0 'base ''Password)
$(deriveSafeCopy 0 'base ''Name)
$(deriveSafeCopy 0 'base ''Desc)
$(deriveSafeCopy 0 'base ''Image)
$(deriveSafeCopy 0 'base ''Icon)
$(deriveSafeCopy 0 'base ''ChanType)
$(deriveSafeCopy 0 'base ''Notification)
$(deriveSafeCopy 0 'base ''Contact)
$(deriveSafeCopy 0 'base ''Channel)
$(deriveSafeCopy 1 'base ''User)
$(deriveSafeCopy 0 'base ''User1)
$(deriveSafeCopy 0 'base ''Message)
$(deriveSafeCopy 0 'base ''NoC)

instance Migrate User where
    type MigrateFrom User = User1
    migrate user1 = User (_id1 user1)
                         (_login1 user1)
                         (_password1 user1)
                         (_name1 user1)
                         (_desc1 user1)
                         (_icon1 user1)
                         (_email1 user1)
                         (_ownedChannels1 user1)
                         (_subscriptions1 user1)
                         (S.map (flip Contact Nothing) (_contacts1 user1))
                         (_notifications1 user1)
