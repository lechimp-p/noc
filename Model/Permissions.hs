module Model.Permissions
where

import Data.Monoid

import Model.BaseTypes
import Model.OpMonad
import Model.Errors
import qualified Model.UnsafeOperations as US
import qualified Model.Channel as C
import qualified Model.User as U
import qualified Model.Message as M

-- permissions abstract

data Permission a = 
      Permission
        (UserId -> a -> Operation Bool) -- definition of the permission
        (UserId -> a -> Operation PermissionViolation)    -- info about violation
    | Forbidden                                 

instance Monoid (Permission a) where
    mempty = Forbidden
    Forbidden `mappend` p = p
    p `mappend` Forbidden = p
    Permission ck ct `mappend` Permission ck' ct'
        = Permission (\ u o -> (ck u o >>= \ r -> if r then return True else ck' u o))
                     (\ u o -> (sequence [ct u o, ct' u o] >>= \ r -> return (mconcat r)))

-- permissions on noc

--forNoCAdmins :: Permission () PermissionViolation e
--forNoCAdmins = Permission (


