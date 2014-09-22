--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Simple
    ( runQuery
    , runQueryAndUpdate
    , runSimple
    , run
    )
where

--import Control.Monad
--import Control.Applicative
import Control.Lens 
--import Data.Data (Data, Typeable)
import qualified Data.Set as S 
import qualified Data.IxSet as IX
--import Data.Maybe (isJust)

--import Model.OpMonad
--import Model.Errors

import Control.Eff

import Model.BaseTypes
import Model.Errors
import Model.Query
import Model.Update
import Model.Exec

import qualified Model.Simple.NoC as N
import Model.Simple.NoC
import qualified Model.Simple.User as U
import Model.Simple.User
import qualified Model.Simple.Channel as C
import Model.Simple.Channel
import qualified Model.Simple.Message as M
import Model.Simple.Message

runQuery :: NoC -> Eff (Query :> r) a -> Eff r a
runQuery noc action = go noc (admin action)
    where
    go _ (Val v) = return v
    go noc (E request) = handleRelay request (go noc) (go noc . performQuery noc)

performQuery noc (IsAdmin uid next) = next (uid `S.member` _admins noc)


runQueryAndUpdate :: NoC -> Eff (Query :> Update :> r) a -> Eff r (NoC, a)
runQueryAndUpdate noc action = go noc (admin action)
    where
    go n (Val v) = return (n, v) 
    go n (E request) = checkQuery n request

    checkQuery n r = either (checkUpdate n) (go n . performQuery n) $ decomp r
    checkUpdate n r = either (passOn n) (performUpdate go n) $ decomp r
    passOn n r = send (flip fmap r) >>= go n 
            
performUpdate go noc (AddAdmin uid next) = 
    go (over admins (S.insert uid) noc) (next ())
performUpdate go noc (CreateUser l pw next) = 
    go (over users (IX.insert user) (set nextUserId nuid noc)) (next uid)
    where
    user = User uid l pw (mkName "") (mkDesc "") Nothing S.empty S.empty S.empty []
    uid  = _nextUserId noc
    nuid = UserId (uiToInt uid + 1)


runSimple :: NoC -> Maybe UserId -> Eff (Query :> Update :> Exec :> r) a 
          -> Eff r (Either Error (NoC, a))
runSimple noc uid action = go noc uid (admin action)
    where
    go n _ (Val v) = return . Right $ (n, v)
    go n u (E request) = checkQuery n u request

    checkQuery n u r = either (checkUpdate n u) (go n u . performQuery n) $ decomp r
    checkUpdate n u r = either (checkExec n u) (performUpdate (flip go u) n) $ decomp r
    checkExec n u r = either (passOn n u) (performExec go n u) $ decomp r
    passOn n u r = send (flip fmap r) >>= go n u

performExec go noc uid (GetOperatorId next)     = go noc uid (next uid) 
performExec go noc uid (ThrowME err next)       = return . Left $ err 
performExec go noc uid (DoLogout next)          = go noc Nothing (next ())
performExec go noc (Just _) (DoLogin l pw next) = return . Left $ AlreadyLoggedIn
performExec go noc uid (DoLogin l pw next)      =
    case res of
        Nothing -> return . Left $ CantLogin l 
        Just id -> go noc res (next id)
    where 
    res = do
        user <- IX.getOne (_users noc IX.@= l)
        if checkPassword pw (U._password user)
            then return (U._id user)
            else fail "password mismatch"

{--data OpContext = OpContext
    { _noc       :: NoC
    , _operator  :: Maybe UserId
    }
    deriving (Data, Typeable)

makeLenses ''OpContext

newtype Operation a = Operation { runOperation :: OpContext -> Either Error (OpContext, a) }

runOp' noc operator action = runOperation action $ OpContext noc operator 

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
    fmap f v = v >>= return . f

instance Applicative Operation where
    pure = return
    f <*> v = do
        f' <- f
        v' <- v
        return $ f' v'

runOp :: NoC -> Login -> Password -> Operation a -> Either Error (NoC, a)
runOp noc l pw op = over (_Right . _1) _noc 
                  . runOp' noc Nothing
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

doLogin' :: Login -> Password -> Operation UserId 
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
                Just uid -> Right $ (s & operator .~ Just uid, uid) 

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
--}  

