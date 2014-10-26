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
$(deriveSafeCopy 1 'extension ''Contact)
$(deriveSafeCopy 0 'base ''Contact0)
$(deriveSafeCopy 0 'base ''Channel)
$(deriveSafeCopy 1 'extension ''User)
$(deriveSafeCopy 0 'base ''User0)
$(deriveSafeCopy 0 'base ''Message)
$(deriveSafeCopy 0 'base ''NoC)

instance Migrate User where
    type MigrateFrom User = User0
    migrate user0 = User (_id0 user0)
                         (_login0 user0)
                         (_password0 user0)
                         (_name0 user0)
                         (_desc0 user0)
                         (_icon0 user0)
                         (_email0 user0)
                         (_ownedChannels0 user0)
                         (_subscriptions0 user0)
                         (S.map (migrate . flip Contact0 Nothing) (_contacts0 user0))
                         (_notifications0 user0)

instance Migrate Contact where
    type MigrateFrom Contact = Contact0
    migrate (Contact0 uid (Just cid)) = Contact uid cid
    migrate (Contact0 uid Nothing) = Contact uid . error $ "Contact to " ++ show uid ++ " needs to be recreated due to migration." 
