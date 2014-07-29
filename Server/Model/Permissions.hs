module Model.Permissions
where

import Data.Monoid
import qualified Data.Set as S

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

-- permission eval

checkAccess :: a -> Permission a -> Operation b -> Operation b
checkAccess cont (Permission check constr) action = do
    oid <- US.getOperatorId
    success <- check oid cont
    if not success 
        then do 
            err <- constr oid cont
            throw $ InsufficientPermissions err
        else do
            action

-- permissions on noc

isNoCAdmin uid = US.getAdmins >>= \as -> return (uid `S.member` as)

forNoCAdmins :: Permission () 
forNoCAdmins = Permission 
    (\ uid _ -> isNoCAdmin uid)  
    (\ uid _ -> return (NoNoCAdmin uid))

-- permissions on channel

isAdminOf uid _  = US.getAdmins >>= \as -> return (uid `S.member` as)

forChanAdmins :: Permission ChanId
forChanAdmins = Permission isAdminOf $ \ u c -> return $ NoChanAdmin u c

isChanXX us uid cid = US.getChannel cid >>= \chan -> return (uid `S.member` (us chan))

forChanXX :: (C.Channel -> S.Set UserId)
          -> (UserId -> ChanId -> PermissionViolation)
          -> Permission ChanId
forChanXX us cs = Permission (isChanXX us) (\ uid cid -> return (cs uid cid))

isOwnerOf = isChanXX C._owners

forChanOwners :: Permission ChanId
forChanOwners = forChanXX C._owners NoChanOwner

isProducerIn = isChanXX C._producers

forChanProducers :: Permission ChanId
forChanProducers = forChanXX C._producers NoChanProducer 

isConsumerIn = isChanXX C._consumers

forChanConsumers :: Permission ChanId
forChanConsumers = forChanXX C._consumers NoChanConsumer

forAllChanPeople = mconcat [forChanConsumers, forChanProducers, forChanOwners, forChanAdmins]
forChanOwnersOrAdmins = forChanOwners `mappend` forChanAdmins
forConsumersOrOwners = mconcat [forChanConsumers, forChanOwners, forChanAdmins]
forProducersOrOwners = mconcat [forChanProducers, forChanOwners, forChanAdmins]

-- permission on users

forUserSelf :: Permission UserId
forUserSelf = Permission
    (\ oid uid -> return (oid == uid))
    (\ oid uid -> return $ NoUserSelf oid uid) 

forUserAdmins :: Permission UserId
forUserAdmins = Permission 
    (\ oid _ -> US.getAdmins >>= \as -> return (oid `S.member` as))
    (\ oid uid -> return (NoUserAdmin oid uid))

forUserSelfOrAdmins = mconcat [forUserSelf, forUserAdmins]
