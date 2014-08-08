{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Errors
where

import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Monoid
import Data.Text
import Data.Data (Data, Typeable)

import Model.BaseTypes

data PermissionViolation =
      JustForbidden
    | NoNoCAdmin UserId
    | NoChanAdmin UserId ChanId
    | NoChanOwner UserId ChanId
    | NoChanProducer UserId ChanId
    | NoChanConsumer UserId ChanId
    | NoUserAdmin UserId UserId
    | NoUserSelf UserId UserId
    | PVAnd PermissionViolation PermissionViolation
    deriving (Show, Eq, Data, Typeable)

$(deriveSafeCopy 0 'base ''PermissionViolation) 

instance Monoid PermissionViolation where
    mempty = JustForbidden
    pv `mappend` pv' = PVAnd pv pv'


data Error =
      InsufficientPermissions PermissionViolation 
    | UnknownChannel ChanId
    | UnknownUser UserId
    | UnknownLogin Text 
    | UnknownMessage MsgId
    | OnlyOneChannelOwnerLeft ChanId
    | OnlyOneNoCAdminLeft
    | DuplicateLogin Login
    | CantLogin Login 
    | AlreadyLoggedIn
    | NotLoggedIn
    | Custom String
    deriving (Show, Eq, Data, Typeable)

$(deriveSafeCopy 0 'base ''Error) 
