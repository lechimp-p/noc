--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Simple
    ( runQuery
    --, runQueryAndUpdate
    --, runSimple
    --, run
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

runQuery :: NoC -> Eff (Query :> Exec :> r) a -> Eff (Exec :> r) a
runQuery noc action = go noc (admin action)
    where
    go _ (Val v) = return v
    go noc (E request) = handleRelay request (go noc) 
        $ \ req -> case evalQuery noc req of 
            Right next -> go noc next 
            Left err -> throwME err

evalQuery :: (Member Query r)
          => NoC -> Query (VE r w) -> Either Error (VE r w) 
evalQuery noc (IsAdmin uid next) = Right . next . S.member uid . _admins $ noc
evalQuery noc (CountAdmins next) = Right . next . S.size . _admins $ noc
evalQuery noc (GetUserIdByLogin l next) = Right . next . fmap U._id . IX.getOne $ _users noc IX.@= l 
evalQuery noc (ChanQuery cid q) = evalChanQuery noc q cid
evalQuery noc (UserQuery uid q) = evalUserQuery noc q uid

--fmap' :: (a -> b) -> Either c a -> Either c b
--fmap' _ (Left v) = Left v
--fmap' f (Right v) = Right (f v) 

evalChanQuery :: (Member Query r)
              => NoC -> ChanQueryType (VE r a) -> ChanId -> Either Error (VE r a) 
evalChanQuery noc (GetChanName next) 
    = fmap next . queryOnChan C._name noc
evalChanQuery noc (GetChanDesc next) 
    = fmap next . queryOnChan C._desc noc 
evalChanQuery noc (GetChanType next) 
    = fmap next . queryOnChan C._type' noc 
evalChanQuery noc (GetChanImage next) 
    = fmap next . queryOnChan C._image noc 
evalChanQuery noc (IsChanOwner uid next) 
    = fmap next . queryOnChan (S.member uid . C._owners) noc
evalChanQuery noc (IsChanProducer uid next) 
    = fmap next . queryOnChan (S.member uid . C._producers) noc 
evalChanQuery noc (IsChanConsumer uid next) 
    = fmap next . queryOnChan (S.member uid . C._consumers) noc
evalChanQuery noc (AmountOfSubscribedUsers next) 
    = fmap next . queryOnChan (S.size . C._subscribers) noc  
evalChanQuery noc (LastPostTimestamp next) 
    = undefined
evalChanQuery noc (Messages ofs am next) 
    = undefined 
evalChanQuery noc (MessagesTill ts next) 
    = undefined 

queryOnChan :: (Channel -> b)
            -> NoC 
            -> ChanId
            -> Either Error b 
queryOnChan fun noc cid = 
    let chan = IX.getOne (_channels noc IX.@= cid)
    in case chan of
        Nothing -> Left $ UnknownChannel cid
        Just c -> Right $ fun c

evalUserQuery :: (Member Query r)
              => NoC -> UserQueryType (VE r a) -> UserId -> Either Error (VE r a) 
evalUserQuery noc (GetUserLogin next) 
    = fmap next . queryOnUser U._login noc
evalUserQuery noc (GetUserName next) 
    = fmap next . queryOnUser U._name noc
evalUserQuery noc (GetUserDesc next) 
    = fmap next . queryOnUser U._desc noc

queryOnUser :: (User -> b)
            -> NoC 
            -> UserId
            -> Either Error b 
queryOnUser fun noc uid = 
    let user = IX.getOne (_users noc IX.@= uid)
    in case user of
        Nothing -> Left $ UnknownUser uid
        Just u -> Right $ fun u


runQueryAndUpdate :: NoC -> Eff (Query :> Update :> Exec :> r) a -> Eff (Exec :> r) (NoC, a)
runQueryAndUpdate noc action = go noc (admin action)
    where
    go n (Val v) = return (n, v) 
    go n (E request) = checkQuery n request

    checkQuery n r = flip (either (checkUpdate n)) (decomp r)
                        $ \ req -> case evalQuery n req of 
                            Right next -> go n next
                            Left err -> throwME err
    checkUpdate n r = flip (either (passOn n)) (decomp r) 
                        $ \ req -> case evalUpdate n req of
                            Right (next, noc') -> go noc' next
                            Left err -> throwME err
    passOn n r = send (flip fmap r) >>= go n 

evalUpdate :: (Member Update r)
           => NoC -> Update (VE r w) => Either Error ((VE r w), NoC)
evalUpdate noc (AddAdmin uid next) = Right (next (), over admins (S.insert uid) noc) 
evalUpdate noc (CreateUser l pw next) = Right (next uid, noc')
    where
    noc' = over users (IX.insert user) (set nextUserId nuid noc)
    user = User uid l pw (mkName "") (mkDesc "") Nothing S.empty S.empty S.empty []
    uid  = _nextUserId noc
    nuid = UserId (uiToInt uid + 1)
evalUpdate noc (ChanUpdate cid q) = evalChanUpdate q noc cid
evalUpdate noc (UserUpdate uid q) = evalUserUpdate q noc uid

evalChanUpdate (SetChanName n next) = updateOnChan (set C.name n) (next ())
evalUserUpdate (SetUserLogin l next) = undefined 

updateOnChan :: (Channel -> Channel)
             -> a
             -> NoC
             -> ChanId
             -> Either Error (a, NoC)
updateOnChan mod v noc cid = 
    let chan = IX.getOne (_channels noc IX.@= cid)
    in case chan of
        Nothing -> Left $ UnknownChannel cid
        Just c -> Right (v, over channels (IX.updateIx cid (mod c)) noc)

runSimple :: NoC -> Maybe UserId -> Eff (Query :> Update :> Exec :> r) a 
          -> Eff r (Either Error (NoC, a))
runSimple noc uid action = go noc uid (admin action)
    where
    go n _ (Val v) = return . Right $ (n, v)
    go n u (E request) = checkQuery n u request

    checkQuery n u r = flip (either (checkUpdate n u)) (decomp r)
                        $ \ req -> case evalQuery n req of 
                            Right next -> go n u next
                            Left err -> return . Left $ err
    checkUpdate n u r = flip (either (checkExec n u)) (decomp r) 
                        $ \ req -> case evalUpdate n req of
                            Right (next, noc') -> go noc' u next
                            Left err -> return . Left $ err
    checkExec n u r = flip (either (passOn n u)) (decomp r)
                        $ \ req -> case evalExec n u req of
                            Right (next, noc', uid') -> go noc' uid' next 
                            Left err -> return . Left $ err
    passOn n u r = send (flip fmap r) >>= go n u

evalExec noc uid (GetOperatorId next)     = Right (next uid, noc, uid) 
evalExec noc uid (ThrowME err next)       = Left $ err 
evalExec noc uid (DoLogout next)          = Right (next (), noc, uid)
evalExec noc (Just _) (DoLogin l pw next) = Left $ AlreadyLoggedIn
evalExec noc uid (DoLogin l pw next)      = 
    case res of
        Nothing -> Left $ CantLogin l 
        Just id -> Right (next id, noc, res)
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

