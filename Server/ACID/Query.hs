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
import Data.Acid ( Query )
import Data.Text ( Text )
import Data.Set ( Set )

import Model 
import qualified Model.Operations as O
import Model.OpMonad 
import Model.Simple (runOp')
import Model.Errors

newtype OpQuery a = OpQuery { runOpQuery :: EitherT Error (StateT (Maybe UserId) (Query NoC)) a }
                        deriving (Functor, Applicative, Monad, MonadError Error)

getQuery :: OpQuery a -> Query NoC (Either Error a)
getQuery = flip evalStateT Nothing . runEitherT . runOpQuery

instance OpMonad OpQuery where
    throw = throwError
    getChannels = onSimple getChannels 
    storeChannel = const (throwMsg "storeChannels is no query.")
    newChanId = throwMsg "newChanId is no query."
    getUsers = onSimple getUsers
    storeUser = const (throwMsg "storeUser is no query.")
    newUserId = throwMsg "newUserId is no query."
    getMessages = onSimple getMessages 
    storeMessage = const (throwMsg "storeMessage is no query.")
    newMsgId = throwMsg "newMsgId is no query."
    getAdmins = onSimple getAdmins
    addAdmin = const (throwMsg "addAdmin is no query.")
    rmAdmin = const (throwMsg "rmAdmin is no query.")
    getOperatorId = OpQuery . lift $ get 
    doLogin l p = onSimple $ doLogin l p
    doLogout = onSimple doLogout 

onSimple op = OpQuery $ do
    noc <- lift . lift $ ask 
    oid <- lift $ get 
    case runOp' noc oid op of
        Left err -> left err
        Right (_, v) -> return v

getOperatorIdQ :: Query NoC (Either Error UserId) 
getOperatorIdQ = getQuery O.getOperatorId

getChanNameQ :: ChanId -> Query NoC (Either Error Name)
getChanNameQ = getQuery . O.getChanName 

getChanDescQ :: ChanId -> Query NoC (Either Error Desc) 
getChanDescQ = getQuery . O.getChanDesc

getUserLoginQ :: UserId -> Query NoC (Either Error Login)
getUserLoginQ = getQuery . O.getUserLogin 

getUserNameQ :: UserId -> Query NoC (Either Error Name)
getUserNameQ = getQuery . O.getUserName

getUserDescQ :: UserId -> Query NoC (Either Error Desc)
getUserDescQ = getQuery . O.getUserDesc

getUserIconQ :: UserId -> Query NoC (Either Error (Maybe Icon))
getUserIconQ = getQuery . O.getUserIcon

getUserOwnedChannelsQ :: UserId -> Query NoC (Either Error (Set ChanId))
getUserOwnedChannelsQ = getQuery . O.getUserOwnedChannels

getUserSubscriptionsQ :: UserId -> Query NoC (Either Error (Set ChanId))
getUserSubscriptionsQ = getQuery . O.getUserSubscriptions

getUserContactsQ :: UserId -> Query NoC (Either Error (Set UserId))
getUserContactsQ = getQuery . O.getUserContacts

getUserByLoginQ :: Text -> Query NoC (Either Error UserId)
getUserByLoginQ = getQuery . O.getUserByLogin

messagesQ :: ChanId -> Offset -> Amount -> Query NoC (Either Error [Message])
messagesQ c o = getQuery . O.messages c o
