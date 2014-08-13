{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ACID.Update
where

import Control.Monad.State.Strict
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class
import Control.Monad.Error.Class hiding (Error)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Either
import Control.Applicative
import Data.Acid ( Update )
import Data.Text ( Text )
import Data.Set ( Set )
import Data.Time.Clock

import Model 
import qualified Model.Operations as O
import Model.OpMonad 
import Model.Simple (runOp', _operator, _noc)
import Model.Errors

newtype OpUpdate a = OpUpdate { runOpUpdate :: EitherT Error (StateT (Maybe UserId) (Update NoC)) a }
                    deriving (Functor, Applicative, Monad, MonadError Error)

getUpdate :: Maybe UserId -> OpUpdate a -> Update NoC (Either Error a, Maybe UserId)
getUpdate oid = flip runStateT oid . runEitherT . runOpUpdate

instance OpMonad OpUpdate where
    throw = throwError
    getChannels = onSimple getChannels 
    storeChannel = onSimple . storeChannel 
    newChanId = onSimple newChanId 
    getUsers = onSimple getUsers
    storeUser = onSimple . storeUser
    newUserId = onSimple newUserId 
    getMessages = onSimple getMessages 
    storeMessage = onSimple . storeMessage 
    newMsgId = onSimple newMsgId
    getAdmins = onSimple getAdmins
    addAdmin = onSimple . Model.OpMonad.addAdmin 
    rmAdmin = onSimple . Model.OpMonad.rmAdmin
    getOperatorId = OpUpdate . lift $ get 
    doLogin l p = onSimple $ doLogin l p
    doLogout = onSimple doLogout 

onSimple op = OpUpdate $ do
    noc <- lift . lift $ get 
    oid <- lift $ get 
    case runOp' noc oid op of
        Left err -> left err
        Right (context, v) -> do
            lift . put . _operator $ context
            lift . lift . put . _noc $ context
            return v

doLoginU :: Login -> Password -> Update NoC (Either Error (), Maybe UserId)
doLoginU l = getUpdate Nothing . doLogin l

getOperatorIdU :: Maybe UserId -> Update NoC (Either Error UserId, Maybe UserId) 
getOperatorIdU oid = getUpdate oid O.getOperatorId

addAdminU :: Maybe UserId -> UserId -> Update NoC (Either Error (), Maybe UserId)
addAdminU oid = getUpdate oid . O.addAdmin 

rmAdminU :: Maybe UserId -> UserId -> Update NoC (Either Error (), Maybe UserId)
rmAdminU oid = getUpdate oid . O.rmAdmin 


getChanNameU :: Maybe UserId -> ChanId -> Update NoC (Either Error Name, Maybe UserId)
getChanNameU oid = getUpdate oid . O.getChanName 

getChanDescU :: Maybe UserId -> ChanId -> Update NoC (Either Error Desc, Maybe UserId) 
getChanDescU oid = getUpdate oid . O.getChanDesc

setChanNameU :: Maybe UserId -> ChanId -> Name -> Update NoC (Either Error (), Maybe UserId)
setChanNameU oid cid = getUpdate oid . O.setChanName cid

setChanDescU :: Maybe UserId -> ChanId -> Desc -> Update NoC (Either Error (), Maybe UserId) 
setChanDescU oid cid = getUpdate oid . O.setChanDesc cid


addChanOwnerU :: Maybe UserId -> ChanId -> UserId -> Update NoC (Either Error (), Maybe UserId)
addChanOwnerU oid cid = getUpdate oid . O.addChanOwner cid

addChanProducerU :: Maybe UserId -> ChanId -> UserId -> Update NoC (Either Error (), Maybe UserId)
addChanProducerU oid cid = getUpdate oid . O.addChanProducer cid

addChanConsumerU :: Maybe UserId -> ChanId -> UserId -> Update NoC (Either Error (), Maybe UserId)
addChanConsumerU oid cid = getUpdate oid . O.addChanConsumer cid

rmChanOwnerU :: Maybe UserId -> ChanId -> UserId -> Update NoC (Either Error (), Maybe UserId)
rmChanOwnerU oid cid = getUpdate oid . O.rmChanOwner cid

rmChanProducerU :: Maybe UserId -> ChanId -> UserId -> Update NoC (Either Error (), Maybe UserId)
rmChanProducerU oid cid = getUpdate oid . O.rmChanProducer cid

rmChanConsumerU :: Maybe UserId -> ChanId -> UserId -> Update NoC (Either Error (), Maybe UserId)
rmChanConsumerU oid cid = getUpdate oid . O.rmChanConsumer cid


getUserLoginU :: Maybe UserId -> UserId -> Update NoC (Either Error Login, Maybe UserId)
getUserLoginU oid = getUpdate oid . O.getUserLogin 

getUserNameU :: Maybe UserId -> UserId -> Update NoC (Either Error Name, Maybe UserId)
getUserNameU oid = getUpdate oid . O.getUserName

getUserDescU :: Maybe UserId -> UserId -> Update NoC (Either Error Desc, Maybe UserId)
getUserDescU oid = getUpdate oid . O.getUserDesc

getUserIconU :: Maybe UserId -> UserId -> Update NoC (Either Error (Maybe Icon), Maybe UserId)
getUserIconU oid = getUpdate oid . O.getUserIcon

setUserLoginU :: Maybe UserId -> UserId -> Login -> Update NoC (Either Error (), Maybe UserId)
setUserLoginU oid uid = getUpdate oid . O.setUserLogin uid

setUserPasswordU :: Maybe UserId -> UserId -> Password -> Update NoC (Either Error (), Maybe UserId)
setUserPasswordU oid uid = getUpdate oid . O.setUserPassword uid

setUserNameU :: Maybe UserId -> UserId -> Name -> Update NoC (Either Error (), Maybe UserId)
setUserNameU oid uid = getUpdate oid . O.setUserName uid

setUserDescU :: Maybe UserId -> UserId -> Desc -> Update NoC (Either Error (), Maybe UserId)
setUserDescU oid uid = getUpdate oid . O.setUserDesc uid

setUserIconU :: Maybe UserId -> UserId -> Maybe Icon -> Update NoC (Either Error (), Maybe UserId)
setUserIconU oid uid = getUpdate oid . O.setUserIcon uid

getUserOwnedChannelsU :: Maybe UserId -> UserId -> Update NoC (Either Error (Set ChanId), Maybe UserId)
getUserOwnedChannelsU oid = getUpdate oid . O.getUserOwnedChannels

getUserSubscriptionsU :: Maybe UserId -> UserId -> Update NoC (Either Error (Set ChanId), Maybe UserId)
getUserSubscriptionsU oid = getUpdate oid . O.getUserSubscriptions

getUserContactsU :: Maybe UserId -> UserId -> Update NoC (Either Error (Set UserId), Maybe UserId)
getUserContactsU oid = getUpdate oid . O.getUserContacts

getUserByLoginU :: Maybe UserId -> Text -> Update NoC (Either Error UserId, Maybe UserId)
getUserByLoginU oid = getUpdate oid . O.getUserByLogin

createUserU :: Maybe UserId -> Login -> Password -> Update NoC (Either Error UserId, Maybe UserId)
createUserU oid l = getUpdate oid . O.createUser l

createChannelU :: Maybe UserId -> Name -> Desc -> Update NoC (Either Error ChanId, Maybe UserId)
createChannelU oid n = getUpdate oid . O.createChannel n

postU :: Maybe UserId -> ChanId -> UTCTime -> Text -> Maybe Image -> Update NoC (Either Error MsgId, Maybe UserId)
postU oid cid ts t = getUpdate oid . O.post cid ts t

messagesU :: Maybe UserId -> ChanId -> Offset -> Amount -> Update NoC (Either Error [Message], Maybe UserId)
messagesU oid c o = getUpdate oid . O.messages c o
