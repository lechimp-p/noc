module Model.Acid.Query
where

import Model.BaseTypes
import Model.Message
import Model.Errors
import Model.Simple.Operations
import Model.Acid.SafeCopy

import Data.Acid
import Data.Text (Text)
import qualified Data.Set as S
import Control.Monad.Reader (ask)
import Data.Time.Clock (UTCTime)


qDoLogin :: Login -> Password -> Query NoC (Maybe UserId)
qDoLogin l pw = ask >>= \ noc -> return $ doLoginR noc l pw

qIsAdmin :: UserId -> Query NoC (Either Error Bool)
qIsAdmin uid = ask >>= \ noc -> return . Right $ isAdminR noc uid

qCountAdmins :: Query NoC (Either Error Int)
qCountAdmins = ask >>= \ noc -> return . Right $ countAdminsR noc

qGetUserIdByLogin :: Text -> Query NoC (Either Error (Maybe UserId))
qGetUserIdByLogin t = ask >>= \ noc -> return . Right $ getUserIdByLoginR noc t

qGetChanName :: ChanId -> Query NoC (Either Error Name)
qGetChanName cid = ask >>= \ noc -> return $ getChanNameR noc cid

qGetChanDesc :: ChanId -> Query NoC (Either Error Desc)
qGetChanDesc cid = ask >>= \ noc -> return $ getChanDescR noc cid

qGetChanType :: ChanId -> Query NoC (Either Error ChanType)
qGetChanType cid = ask >>= \ noc -> return $ getChanTypeR noc cid

qGetChanImage :: ChanId -> Query NoC (Either Error (Maybe Image))
qGetChanImage cid = ask >>= \ noc -> return $ getChanImageR noc cid

qIsChanOwner :: ChanId -> UserId -> Query NoC (Either Error Bool)
qIsChanOwner cid uid = ask >>= \ noc -> return $ isChanOwnerR noc cid uid

qIsChanProducer :: ChanId -> UserId -> Query NoC (Either Error Bool)
qIsChanProducer cid uid = ask >>= \ noc -> return $ isChanProducerR noc cid uid

qIsChanConsumer :: ChanId -> UserId -> Query NoC (Either Error Bool)
qIsChanConsumer cid uid = ask >>= \ noc -> return $ isChanConsumerR noc cid uid

qAmountOfSubscribedUsers :: ChanId -> Query NoC (Either Error Int)
qAmountOfSubscribedUsers cid = ask >>= \ noc -> return $ amountOfSubscribedUsersR noc cid

qLastPostTimestamp :: ChanId -> Query NoC (Either Error (Maybe UTCTime))
qLastPostTimestamp cid = ask >>= \ noc -> return $ lastPostTimestampR noc cid

qMessages :: ChanId -> Offset -> Amount -> Query NoC (Either Error [Message])
qMessages cid ofs am = ask >>= \ noc -> return $ messagesR noc cid ofs am

qMessagesTill :: ChanId -> UTCTime -> Query NoC (Either Error [Message])
qMessagesTill cid ts = ask >>= \ noc -> return $ messagesTillR noc cid ts

qGetUserLogin :: UserId -> Query NoC (Either Error Login)
qGetUserLogin uid = ask >>= \ noc -> return $ getUserLoginR noc uid

qGetUserName :: UserId -> Query NoC (Either Error Name)
qGetUserName uid = ask >>= \ noc -> return $ getUserNameR noc uid

qGetUserDesc :: UserId -> Query NoC (Either Error Desc)
qGetUserDesc uid = ask >>= \ noc -> return $ getUserDescR noc uid

qGetUserIcon :: UserId -> Query NoC (Either Error (Maybe Icon))
qGetUserIcon uid = ask >>= \ noc -> return $ getUserIconR noc uid

qGetUserNotifications :: UserId -> Query NoC (Either Error [Notification])
qGetUserNotifications uid = ask >>= \ noc -> return $ getUserNotificationsR noc uid

qGetUserContacts :: UserId -> Query NoC (Either Error (S.Set UserId))
qGetUserContacts uid = ask >>= \ noc -> return $ getUserContactsR noc uid

qGetUserSubscriptions :: UserId -> Query NoC (Either Error (S.Set ChanId))
qGetUserSubscriptions uid = ask >>= \ noc -> return $ getUserSubscriptionsR noc uid
