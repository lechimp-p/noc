module Model.Permissions
where

import Data.Monoid
import qualified Data.Set as S

import Model.BaseTypes
import Model.OpMonad
import Model.Errors
import qualified Model.Channel as C
import qualified Model.User as U
import qualified Model.Message as M

-- permissions abstract

data Permission a m = 
      Permission
        (UserId -> a -> m Bool)                   -- definition of the permission
        (UserId -> a -> m PermissionViolation)    -- info about violation
    | Forbidden                                 

instance OpMonad m => Monoid (Permission a m) where
    mempty = Forbidden
    Forbidden `mappend` p = p
    p `mappend` Forbidden = p
    Permission ck ct `mappend` Permission ck' ct'
        = Permission (\ u o -> (ck u o >>= \ r -> if r then return True else ck' u o))
                     (\ u o -> (sequence [ct u o, ct' u o] >>= \ r -> return (mconcat r)))

-- permission eval

checkAccess :: OpMonad m => a -> Permission a m -> m b -> m b
checkAccess cont (Permission check constr) action = ifIsLoggedIn' $ \oid -> do
    success <- check oid cont 
    if not success
        then constr oid cont >>= throw . InsufficientPermissions 
        else action

ifIsLoggedIn' :: OpMonad m => (UserId -> m b) -> m b 
ifIsLoggedIn' op = do
    oid <- getOperatorId
    case oid of
        Nothing  -> throw NotLoggedIn
        Just oid -> op oid 

ifIsLoggedIn :: OpMonad m => m b -> m b
ifIsLoggedIn = ifIsLoggedIn' . const 

-- permissions on noc

isNoCAdmin uid = getAdmins >>= \as -> return (uid `S.member` as)

forNoCAdmins :: OpMonad m => Permission () m 
forNoCAdmins = Permission 
    (\ uid _ -> isNoCAdmin uid)  
    (\ uid _ -> return (NoNoCAdmin uid))

-- permissions on channel

isAdminOf uid _  = getAdmins >>= \as -> return (uid `S.member` as)

forChanAdmins :: OpMonad m => Permission ChanId m
forChanAdmins = Permission isAdminOf $ \ u c -> return $ NoChanAdmin u c

isChanXX us uid cid = getChannel cid >>= \chan -> return (uid `S.member` (us chan))

forChanXX :: OpMonad m 
          => (C.Channel -> S.Set UserId)
          -> (UserId -> ChanId -> PermissionViolation)
          -> Permission ChanId m
forChanXX us cs = Permission (isChanXX us) (\ uid cid -> return (cs uid cid))

isOwnerOf :: OpMonad m => UserId -> ChanId -> m Bool
isOwnerOf = isChanXX C._owners

forChanOwners :: OpMonad m => Permission ChanId m
forChanOwners = forChanXX C._owners NoChanOwner

isProducerIn :: OpMonad m => UserId -> ChanId -> m Bool
isProducerIn = isChanXX C._producers

forChanProducers :: OpMonad m => Permission ChanId m
forChanProducers = forChanXX C._producers NoChanProducer 

isConsumerIn :: OpMonad m => UserId -> ChanId -> m Bool
isConsumerIn = isChanXX C._consumers

forChanConsumers :: OpMonad m => Permission ChanId m
forChanConsumers = forChanXX C._consumers NoChanConsumer

forAllChanPeople :: OpMonad m => Permission ChanId m
forAllChanPeople = mconcat [forChanConsumers, forChanProducers, forChanOwners, forChanAdmins]
forChanOwnersOrAdmins :: OpMonad m => Permission ChanId m
forChanOwnersOrAdmins = forChanOwners `mappend` forChanAdmins
forConsumersOrOwners :: OpMonad m => Permission ChanId m
forConsumersOrOwners = mconcat [forChanConsumers, forChanOwners, forChanAdmins]
forProducersOrOwners :: OpMonad m => Permission ChanId m
forProducersOrOwners = mconcat [forChanProducers, forChanOwners, forChanAdmins]

-- permission on users

forUserSelf :: OpMonad m => Permission UserId m
forUserSelf = Permission
    (\ oid uid -> return (oid == uid))
    (\ oid uid -> return $ NoUserSelf oid uid) 

forUserAdmins :: OpMonad m => Permission UserId m
forUserAdmins = Permission 
    (\ oid _ -> getAdmins >>= \as -> return (oid `S.member` as))
    (\ oid uid -> return (NoUserAdmin oid uid))

forUserSelfOrAdmins :: OpMonad m => Permission UserId m
forUserSelfOrAdmins = mconcat [forUserSelf, forUserAdmins]
