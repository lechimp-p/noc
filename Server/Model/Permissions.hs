{-# LANGUAGE FlexibleContexts #-}

module Model.Permissions
where

import Data.Monoid
import qualified Data.Set as S
import Control.Eff

import Model.BaseTypes
import Model.Exec
import Model.Query
import Model.Errors

-- permissions abstract

data Permission a r = 
      Permission
        (UserId -> a -> Eff r Bool)                   -- definition of the permission
        (UserId -> a -> Eff r PermissionViolation)    -- info about violation
    | Forbidden                                 

instance Monoid (Permission a r) where
    mempty = Forbidden
    Forbidden `mappend` p = p
    p `mappend` Forbidden = p
    Permission ck ct `mappend` Permission ck' ct'
        = Permission (\ u o -> (ck u o >>= \ r -> if r then return True else ck' u o))
                     (\ u o -> (sequence [ct u o, ct' u o] >>= \ r -> return (mconcat r)))

-- permission eval

checkAccess :: (Member Exec r, Member Query r) 
            => a -> Permission a r -> Eff r () 
checkAccess cd (Permission ck err) = do
    oid <- forceOperatorId
    success <- ck oid cd 
    if not success
        then err oid cd >>= throwME . InsufficientPermissions 
        else return ()

tryAccess :: (Member Exec r, Member Query r)
          => a -> Permission a r -> Eff r b -> Eff r (Maybe b)
tryAccess cd (Permission ck _) act = do
    oid <- getOperatorId
    case oid of
        Nothing -> return Nothing
        Just oid' -> do
            success <- ck oid' cd 
            if success
                then do
                    res <- act 
                    return $ Just res
                else return Nothing 

forceOperatorId :: (Member Exec r)
                => Eff r UserId
forceOperatorId = do
    oid <- getOperatorId
    case oid of
        Nothing -> throwME NotLoggedIn
        Just o -> return o

-- permissions on noc

forAdmins :: Member Query r => Permission a r 
forAdmins = Permission 
    (\ uid _ -> isAdmin uid)  
    (\ uid _ -> return (NoNoCAdmin uid))

-- permissions on channel

ret e = \ a b -> return $ e a b

forChanOwners :: Member Query r => Permission ChanId r
forChanOwners = Permission (flip isChanOwner) (ret NoChanOwner)

forChanProducers :: Member Query r => Permission ChanId r
forChanProducers = Permission (flip isChanProducer) (ret NoChanProducer)

forChanConsumers :: Member Query r => Permission ChanId r
forChanConsumers = Permission (flip isChanConsumer) (ret NoChanConsumer)

forAllChanPeople :: Member Query r => Permission ChanId r
forAllChanPeople = mconcat [forChanConsumers, forChanProducers, forChanOwners, forAdmins]

forChanOwnersOrAdmins :: Member Query r => Permission ChanId r
forChanOwnersOrAdmins = forChanOwners `mappend` forAdmins

forConsumersOrOwners :: Member Query r => Permission ChanId r
forConsumersOrOwners = mconcat [forChanConsumers, forChanOwners, forAdmins]

forProducersOrOwners :: Member Query r => Permission ChanId r 
forProducersOrOwners = mconcat [forChanProducers, forChanOwners, forAdmins]

-- permission on users

forUserSelf :: Permission UserId r
forUserSelf = Permission (\ o -> return . (==) o) (ret NoUserSelf)

forUsersOnContactList :: Member Query m => Permission UserId m
forUsersOnContactList = Permission
    (\ oid uid -> (getUserContacts uid >>= \ u -> return (oid `S.member` u)))
    (ret NotOnContactList)

forUserSelfOrAdmins :: Member Query m => Permission UserId m
forUserSelfOrAdmins = mconcat [forUserSelf, forAdmins]

forUserContactsSelfOrAdmins :: Member Query m => Permission UserId m
forUserContactsSelfOrAdmins = mconcat [forUsersOnContactList, forUserSelf, forAdmins]
