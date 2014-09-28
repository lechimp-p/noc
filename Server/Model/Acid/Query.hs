{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Acid.Query
where

import Model.BaseTypes
import Model.Message
import Model.Errors
import Model.Simple.Operations
import Model.Acid.Safecopy

import Data.Acid
import Data.Text (Text)
import qualified Data.Set as S
import Control.Monad.Reader (ask)
import Data.Time.Clock (UTCTime)

isAdmin :: UserId -> Query NoC (Either Error Bool)
isAdmin uid = ask >>= \ noc -> return . Right $ isAdminR noc uid

countAdmins :: Query NoC (Either Error Int)
countAdmins = ask >>= \ noc -> return . Right $ countAdminsR noc

getUserIdByLogin :: Text -> Query NoC (Either Error (Maybe UserId))
getUserIdByLogin t = ask >>= \ noc -> return . Right $ getUserIdByLoginR noc t

getChanName :: ChanId -> Query NoC (Either Error Name)
getChanName cid = ask >>= \ noc -> return $ getChanNameR noc cid

getChanDesc :: ChanId -> Query NoC (Either Error Desc)
getChanDesc cid = ask >>= \ noc -> return $ getChanDescR noc cid

getChanType :: ChanId -> Query NoC (Either Error ChanType)
getChanType cid = ask >>= \ noc -> return $ getChanTypeR noc cid

getChanImage :: ChanId -> Query NoC (Either Error (Maybe Image))
getChanImage cid = ask >>= \ noc -> return $ getChanImageR noc cid

isChanOwner :: ChanId -> UserId -> Query NoC (Either Error Bool)
isChanOwner cid uid = ask >>= \ noc -> return $ isChanOwnerR noc cid uid

isChanProducer :: ChanId -> UserId -> Query NoC (Either Error Bool)
isChanProducer cid uid = ask >>= \ noc -> return $ isChanProducerR noc cid uid

isChanConsumer :: ChanId -> UserId -> Query NoC (Either Error Bool)
isChanConsumer cid uid = ask >>= \ noc -> return $ isChanConsumerR noc cid uid

amountOfSubscribedUsers :: ChanId -> Query NoC (Either Error Int)
amountOfSubscribedUsers cid = ask >>= \ noc -> return $ amountOfSubscribedUsersR noc cid

lastPostTimestamp :: ChanId -> Query NoC (Either Error (Maybe UTCTime))
lastPostTimestamp cid = ask >>= \ noc -> return $ lastPostTimestampR noc cid

messages :: ChanId -> Offset -> Amount -> Query NoC (Either Error [Message])
messages cid ofs am = ask >>= \ noc -> return $ messagesR noc cid ofs am

messagesTill :: ChanId -> UTCTime -> Query NoC (Either Error [Message])
messagesTill cid ts = ask >>= \ noc -> return $ messagesTillR noc cid ts

getUserLogin :: UserId -> Query NoC (Either Error Login)
getUserLogin uid = ask >>= \ noc -> return $ getUserLoginR noc uid

getUserName :: UserId -> Query NoC (Either Error Name)
getUserName uid = ask >>= \ noc -> return $ getUserNameR noc uid

getUserDesc :: UserId -> Query NoC (Either Error Desc)
getUserDesc uid = ask >>= \ noc -> return $ getUserDescR noc uid

getUserIcon :: UserId -> Query NoC (Either Error (Maybe Icon))
getUserIcon uid = ask >>= \ noc -> return $ getUserIconR noc uid

getUserNotifications :: UserId -> Query NoC (Either Error [Notification])
getUserNotifications uid = ask >>= \ noc -> return $ getUserNotificationsR noc uid

getUserContacts :: UserId -> Query NoC (Either Error (S.Set UserId))
getUserContacts uid = ask >>= \ noc -> return $ getUserContactsR noc uid

getUserSubscriptions :: UserId -> Query NoC (Either Error (S.Set ChanId))
getUserSubscriptions uid = ask >>= \ noc -> return $ getUserSubscriptionsR noc uid

$(makeAcidic ''NoC  [ 'isAdmin
                    , 'countAdmins
                    , 'getUserIdByLogin
                    , 'getChanName
                    , 'getChanDesc
                    , 'getChanType
                    , 'getChanImage
                    , 'isChanOwner
                    , 'isChanProducer
                    , 'isChanConsumer
                    , 'amountOfSubscribedUsers
                    , 'lastPostTimestamp
                    , 'messages
                    , 'messagesTill
                    , 'getUserLogin
                    , 'getUserName
                    , 'getUserIcon
                    , 'getUserNotifications
                    , 'getUserContacts
                    , 'getUserSubscriptions
                    ])

{--
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

doLoginQ :: Login -> Password -> Query NoC (Either Error UserId, Maybe UserId)
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

messagesTillQ :: Maybe UserId -> ChanId -> UTCTime -> Query NoC (Either Error [Message], Maybe UserId)
messagesTillQ oid c = getQuery oid . O.messagesTill c
--}
