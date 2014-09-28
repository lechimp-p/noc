{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Acid.Safecopy
    ( module Model.Acid.Safecopy
    , module Model.Simple.NoC
    )
where

import Model.BaseTypes
import Model.Errors
import Model.Simple.Channel
import Model.Simple.User
import Model.Simple.Message
import Model.Simple.NoC

import Data.SafeCopy (base, deriveSafeCopy)

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
$(deriveSafeCopy 0 'base ''Channel)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Message)
$(deriveSafeCopy 0 'base ''NoC)
