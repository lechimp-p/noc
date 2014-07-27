module Model.Errors
where

import Data.Monoid

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

instance Monoid PermissionViolation where
    mempty = JustForbidden
    pv `mappend` pv' = PVAnd pv pv'


data Error =
      InsufficientPermissions PermissionViolation 
    | UnknownChannel ChanId
    | UnknownUser UserId
    | UnknownMessage MsgId
