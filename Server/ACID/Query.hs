{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ACID.Query
where

import Control.Monad.State.Strict
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class
import Control.Monad.Error.Class hiding (Error)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Either
import Control.Applicative
import Data.Acid 
        ( AcidState, makeAcidic
        , Query, Update
        , QueryEvent, UpdateEvent
        , EventState, EventResult
        )

import Model (NoC, UserId)
import Model.OpMonad 
import Model.Simple (runOp')
import Model.Errors

newtype OpQuery a = OpQuery { runOpQuery :: EitherT Error (StateT (Maybe UserId) (Query NoC)) a }
                        deriving (Functor, Applicative, Monad, MonadError Error)

instance OpMonad OpQuery where
    throw = throwError
    getChannels = onSimple getChannels 
    storeChannel = error "storeChannels is no query."
    newChanId = error "newChanId is no query."
    getUsers = onSimple getUsers
    storeUser = error "storeUser is no query."
    newUserId = error "newUserId is no query."
    getMessages = onSimple getMessages 
    storeMessage = error "storeMessage is no query."
    newMsgId = error "newMsgId is no query."
    getAdmins = onSimple getAdmins
    addAdmin = error "addAdmin is no query."
    rmAdmin = error "rmAdmin is no query."
    getOperatorId = OpQuery . lift $ get 
    doLogin l p = onSimple $ doLogin l p
    doLogout = onSimple doLogout 

onSimple op = OpQuery $ do
    noc <- lift . lift $ ask 
    oid <- lift $ get 
    case runOp' noc oid op of
        Left err -> left err
        Right (_, v) -> return v
