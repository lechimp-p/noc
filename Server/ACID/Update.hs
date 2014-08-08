{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ACID.Update
where

import Control.Monad.State.Strict
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class
import Control.Monad.Error.Class hiding (Error)
import Control.Monad.State.Class
import Control.Monad.Trans.Either
import Control.Applicative
import Data.Acid ( Update )
import Control.Lens

import Model (NoC, UserId)
import Model.OpMonad 
import Model.Simple (runOp', noc)
import Model.Errors

newtype OpUpdate a = OpUpdate { runOpUpdate :: EitherT Error (StateT (Maybe UserId) (Update NoC)) a }
                        deriving (Functor, Applicative, Monad, MonadError Error)

getUpdate :: OpUpdate a -> Update NoC (Either Error a)
getUpdate = flip evalStateT Nothing . runEitherT . runOpUpdate

instance OpMonad OpUpdate where
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
    getOperatorId = OpUpdate . lift $ get 
    doLogin l p = onSimple $ doLogin l p
    doLogout = onSimple doLogout 

onSimple op = OpUpdate $ do
    n <- lift . lift $ get 
    oid <- lift $ get 
    case runOp' n oid op of
        Left err -> left err
        Right (s, v) -> do
            lift . lift . put $ view noc s 
            return v
