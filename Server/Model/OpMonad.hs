{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.OpMonad
where

import Control.Monad
import Control.Applicative
import Control.Lens 
import Data.Data (Data, Typeable)
import qualified Data.Set as S 
import qualified Data.IxSet as IX
import Data.Maybe (isJust)

import Model.OpMonadType
import Model.Errors
import Model.BaseTypes
import qualified Model.NoC as N
import Model.NoC
import qualified Model.User as U
import Model.User
import qualified Model.Channel as C
import Model.Channel
import qualified Model.Message as M
import Model.Message

data OpContext = OpContext
    { _noc       :: NoC
    , _operator  :: Maybe UserId
    }
    deriving (Data, Typeable)

makeLenses ''OpContext

newtype Operation a = Operation { runOperation :: OpContext -> Either Error (OpContext, a) }

runOp' noc action = runOperation action $ OpContext noc Nothing 

instance Monad Operation where
    return v = Operation $ \ s -> Right (s, v)
    m >>= m' = Operation $ \ s ->
        let l = runOperation m s
        in case l of
            (Left e) -> Left e
            (Right (s', v)) ->
                let n = m' v
                in runOperation n s'

instance Functor Operation where
    fmap f v = do
        v' <- v
        return $ f v'

instance Applicative Operation where
    pure = return
    f <*> v = do
        f' <- f
        v' <- v
        return $ f' v'

runOp :: NoC -> Login -> Password -> Operation a -> Either Error (NoC, a)
runOp noc l pw op = over (_Right . _1) _noc 
                  . runOp' noc 
                  $ doLogin l pw >> op


instance OpMonad Operation where
    throw e = Operation $ \ s -> Left e
    getChannels = getChannels'
    storeChannel = storeChannel'
    newChanId = newChanId'
    getUsers = getUsers'
    storeUser = storeUser'
    newUserId = newUserId'
    getMessages = getMessages'
    storeMessage = storeMessage'
    newMsgId = newMsgId'
    getAdmins = getAdmins'
    addAdmin = addAdmin'
    rmAdmin = rmAdmin'
    getOperatorId = getOperatorId'
    doLogin = doLogin'
    doLogout = doLogout'

doLogin' :: Login -> Password -> Operation ()
doLogin' l pw = do
    oid <- getOperatorId'
    AlreadyLoggedIn `throwOn` isJust oid
    Operation $ \ s -> 
        let maybeUserId = do
                user <- IX.getOne ((s ^. noc . users) IX.@= l)
                if checkPassword (U._password user) pw
                    then return (U._id user)
                    else fail "password mismatch" 
        in case maybeUserId of
                Nothing  -> Left $ CantLogin l
                Just uid -> Right $ (s & operator .~ Just uid, ())  

doLogout' :: Operation ()
doLogout' = Operation $ \ s -> Right (set operator Nothing s, ())


getChannels' :: Operation (IX.IxSet Channel) 
getChannels' = Operation $ \ s -> Right (s, s ^. noc . channels)

storeChannel' :: Channel -> Operation ()
storeChannel' chan = Operation $ \ s -> 
    let s' = over (noc . channels) (IX.insert chan . IX.deleteIx (C._id chan)) s
    in Right (s', ())

newChanId' :: Operation ChanId 
newChanId' = Operation $ \ s ->
    let cid = s ^. noc . nextChanId
        s' = set (noc . nextChanId) (ChanId (1 + ciToInt cid)) s
    in Right (s', cid)

getUsers' :: Operation (IX.IxSet User)
getUsers' = Operation $ \ s -> Right (s, s ^. noc . users)

storeUser' :: User -> Operation ()
storeUser' user = Operation $ \ s -> 
    let s' = over (noc . users ) (IX.insert user . IX.deleteIx (U._id user)) s
    in Right (s', ())

newUserId' :: Operation UserId 
newUserId' = Operation $ \ s ->
    let uid = s ^. noc . nextUserId
        s' = set (noc . nextUserId) (UserId (1 + uiToInt uid)) s
    in Right (s', uid)

getMessages' :: Operation (IX.IxSet Message) 
getMessages' = Operation $ \ s -> Right (s, s ^. noc . N.messages)

storeMessage' :: Message -> Operation ()
storeMessage' msg = Operation $ \ s -> 
    let s' = over (noc . N.messages) (IX.insert msg . IX.deleteIx (M._id msg)) s
    in Right (s', ())

newMsgId' :: Operation MsgId
newMsgId' = Operation $ \ s ->
    let mid = s ^. noc . nextMsgId
        s' = set (noc . nextMsgId) (MsgId (1 + miToInt mid)) s
    in Right (s', mid)

getAdmins' :: Operation (S.Set UserId)
getAdmins' = Operation $ \ s -> Right (s, s ^. noc . admins)

addAdmin' :: UserId -> Operation ()
addAdmin' uid = Operation $ \ s ->
    let s' = over (noc . admins) (S.insert uid) s
    in Right (s', ())

rmAdmin' :: UserId -> Operation ()
rmAdmin' uid = Operation $ \ s ->
    let s' = over (noc . admins) (S.delete uid) s
    in Right (s', ())

getOperatorId' :: Operation (Maybe UserId)
getOperatorId' = Operation $ \ s -> Right (s, _operator s)


