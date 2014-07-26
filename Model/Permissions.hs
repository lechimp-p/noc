module Model.Permissions
where

import Data.Monoid

import Model.BaseTypes
import Model.OpMonad
import qualified Model.Channel as C
import qualified Model.User as U
import qualified Model.Message as M

-- permissions abstract

data Permission a b e = 
      Permission
        (UserId -> a -> Operation e Bool) -- definition of the permission
        (UserId -> a -> Operation e b)    -- info about violation
    | Forbidden                                 

instance Monoid b => Monoid (Permission a b e) where
    mempty = Forbidden
    Forbidden `mappend` p = p
    p `mappend` Forbidden = p
    Permission ck ct `mappend` Permission ck' ct'
        = Permission (\ u o -> (ck u o >>= \ r -> if r then return True else ck' u o))
                     (\ u o -> (sequence [ct u o, ct' u o] >>= \ r -> return (mconcat r)))

-- permissions concrete 

data PermissionViolation =
      JustForbidden
    | PVAnd PermissionViolation PermissionViolation

instance Monoid PermissionViolation where
    mempty = JustForbidden
    pv `mappend` pv' = PVAnd pv pv'
