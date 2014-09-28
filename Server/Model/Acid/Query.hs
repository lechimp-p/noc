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
                    , 'getUserDesc
                    , 'getUserIcon
                    , 'getUserNotifications
                    , 'getUserContacts
                    , 'getUserSubscriptions
                    ])
