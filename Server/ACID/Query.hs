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
import Data.Time.Clock (UTCTime)

import Model 
import qualified Model.Operations as O
import Model.OpMonad 
import Model.Simple (runOp', _operator)
import Model.Errors

newtype OpQuery a = OpQuery { runOpQuery :: EitherT Error (StateT (Maybe UserId) (Query NoC)) a }
                    deriving (Functor, Applicative, Monad, MonadError Error)

getQuery :: Maybe UserId -> OpQuery a -> Query NoC (Either Error a, Maybe UserId)
getQuery oid = flip runStateT oid . runEitherT . runOpQuery

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
        Right (context, v) -> do
            lift . put . _operator $ context
            return v

doLoginQ :: Login -> Password -> Query NoC (Either Error (), Maybe UserId)
doLoginQ l = getQuery Nothing . doLogin l

getOperatorIdQ :: Maybe UserId -> Query NoC (Either Error UserId, Maybe UserId) 
getOperatorIdQ oid = getQuery oid O.getOperatorId

getChanNameQ :: Maybe UserId -> ChanId -> Query NoC (Either Error Name, Maybe UserId)
getChanNameQ oid = getQuery oid . O.getChanName 

getChanDescQ :: Maybe UserId -> ChanId -> Query NoC (Either Error Desc, Maybe UserId) 
getChanDescQ oid = getQuery oid . O.getChanDesc

getChanTypeQ :: Maybe UserId -> ChanId -> Query NoC (Either Error ChanType, Maybe UserId)
getChanTypeQ oid = getQuery oid . O.getChanType

amountOfDistinctUsersQ :: Maybe UserId -> ChanId -> Query NoC (Either Error Int, Maybe UserId)
amountOfDistinctUsersQ oid = getQuery oid . O.amountOfDistinctUsers

lastPostTimestampQ :: Maybe UserId -> ChanId -> Query NoC (Either Error (Maybe UTCTime), Maybe UserId)
lastPostTimestampQ oid = getQuery oid . O.lastPostTimestamp

getUserLoginQ :: Maybe UserId -> UserId -> Query NoC (Either Error Login, Maybe UserId)
getUserLoginQ oid = getQuery oid . O.getUserLogin 

getUserNameQ :: Maybe UserId -> UserId -> Query NoC (Either Error Name, Maybe UserId)
getUserNameQ oid = getQuery oid . O.getUserName

getUserDescQ :: Maybe UserId -> UserId -> Query NoC (Either Error Desc, Maybe UserId)
getUserDescQ oid = getQuery oid . O.getUserDesc

getUserIconQ :: Maybe UserId -> UserId -> Query NoC (Either Error (Maybe Icon), Maybe UserId)
getUserIconQ oid = getQuery oid . O.getUserIcon

getUserOwnedChannelsQ :: Maybe UserId -> UserId -> Query NoC (Either Error (Set ChanId), Maybe UserId)
getUserOwnedChannelsQ oid = getQuery oid . O.getUserOwnedChannels

getUserSubscriptionsQ :: Maybe UserId -> UserId -> Query NoC (Either Error (Set ChanId), Maybe UserId)
getUserSubscriptionsQ oid = getQuery oid . O.getUserSubscriptions

getUserContactsQ :: Maybe UserId -> UserId -> Query NoC (Either Error (Set UserId), Maybe UserId)
getUserContactsQ oid = getQuery oid . O.getUserContacts

getUserNotificationsQ :: Maybe UserId -> UserId -> Offset -> Amount -> Query NoC (Either Error [Notification], Maybe UserId)
getUserNotificationsQ oid uid offs = getQuery oid . O.getUserNotifications uid offs

getUserByLoginQ :: Maybe UserId -> Text -> Query NoC (Either Error UserId, Maybe UserId)
getUserByLoginQ oid = getQuery oid . O.getUserByLogin

messagesQ :: Maybe UserId -> ChanId -> Offset -> Amount -> Query NoC (Either Error [Message], Maybe UserId)
messagesQ oid c o = getQuery oid . O.messages c o
