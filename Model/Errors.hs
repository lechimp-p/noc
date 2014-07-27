module Model.Errors
where

import Data.Monoid

import Model.BaseTypes

data PermissionViolation =
      JustForbidden
    | NoNoCAdmin UserId
    | PVAnd PermissionViolation PermissionViolation

instance Monoid PermissionViolation where
    mempty = JustForbidden
    pv `mappend` pv' = PVAnd pv pv'


data Error =
     InsufficientPermissions PermissionViolation 
