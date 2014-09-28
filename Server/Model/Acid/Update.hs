{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Acid.Update
where

import Model.BaseTypes
import Model.Message
import Model.Errors
import Model.Simple.Operations
import Model.Acid.Safecopy

import Data.Acid
import Data.Text (Text)
import qualified Data.Set as S
import Control.Monad.State (get, put)
import Data.Time.Clock (UTCTime)

modify :: (NoC -> Either Error (NoC, a)) -> Update NoC (Either Error a)
modify f = do
    noc <- get 
    case f noc of
        Left err -> return . Left $ err
        Right (noc, res) -> put noc >> return (Right res)

createChan :: UserId -> Name -> Update NoC (Either Error ChanId)
createChan uid name = modify $ \ noc -> Right $ createChanR noc uid name

createUser :: Login -> Password -> Update NoC (Either Error UserId)
createUser l pw = modify $ \ noc -> Right $ createUserR noc l pw

addAdmin :: UserId -> Update NoC (Either Error ())
addAdmin uid = modify $ \ noc -> Right $ addAdminR noc uid

rmAdmin :: UserId -> Update NoC (Either Error ())
rmAdmin uid = modify $ \ noc -> Right $ rmAdminR noc uid

setChanName :: ChanId -> Name -> Update NoC (Either Error ())
setChanName cid n = modify $ \ noc -> setChanNameR noc cid n

setChanDesc :: ChanId -> Desc -> Update NoC (Either Error ())
setChanDesc cid d = modify $ \ noc -> setChanDescR noc cid d

setChanType :: ChanId -> ChanType -> Update NoC (Either Error ())
setChanType cid t = modify $ \ noc -> setChanTypeR noc cid t

setChanImage :: ChanId -> Maybe Image -> Update NoC (Either Error ())
setChanImage cid img = modify $ \ noc -> setChanImageR noc cid img 

addChanOwner :: ChanId -> UserId -> Update NoC (Either Error ())
addChanOwner cid uid = modify $ \ noc -> addChanOwnerR noc cid uid

rmChanOwner :: ChanId -> UserId -> Update NoC (Either Error ())
rmChanOwner cid uid = modify $ \ noc -> rmChanOwnerR noc cid uid

addChanProducer :: ChanId -> UserId -> Update NoC (Either Error ())
addChanProducer cid uid = modify $ \ noc -> addChanProducerR noc cid uid

rmChanProducer :: ChanId -> UserId -> Update NoC (Either Error ())
rmChanProducer cid uid = modify $ \ noc -> rmChanProducerR noc cid uid

addChanConsumer :: ChanId -> UserId -> Update NoC (Either Error ())
addChanConsumer cid uid = modify $ \ noc -> addChanConsumerR noc cid uid

rmChanConsumer :: ChanId -> UserId -> Update NoC (Either Error ())
rmChanConsumer cid uid = modify $ \ noc -> rmChanConsumerR noc cid uid

post :: ChanId -> UserId -> UTCTime -> Text -> Maybe Image -> Update NoC (Either Error MsgId)
post cid uid ts txt img = modify $ \ noc -> postR noc cid uid ts txt img

setUserLogin :: UserId -> Login -> Update NoC (Either Error ())
setUserLogin uid l = modify $ \ noc -> setUserLoginR noc uid l

setUserPassword :: UserId -> Password -> Update NoC (Either Error ())
setUserPassword uid l = modify $ \ noc -> setUserPasswordR noc uid l

setUserName :: UserId -> Name -> Update NoC (Either Error ())
setUserName uid l = modify $ \ noc -> setUserNameR noc uid l

setUserDesc :: UserId -> Desc -> Update NoC (Either Error ())
setUserDesc uid l = modify $ \ noc -> setUserDescR noc uid l

setUserIcon :: UserId -> Maybe Icon -> Update NoC (Either Error ())
setUserIcon uid l = modify $ \ noc -> setUserIconR noc uid l

addUserNotification :: UserId -> Notification -> Update NoC (Either Error ())
addUserNotification uid n = modify $ \ noc -> addUserNotificationR noc uid n

addUserContact :: UserId -> UserId -> Update NoC (Either Error ())
addUserContact uid c = modify $ \ noc -> addUserContactR noc uid c

rmUserContact :: UserId -> UserId -> Update NoC (Either Error ())
rmUserContact uid c = modify $ \ noc -> rmUserContactR noc uid c

addUserSubscription :: UserId -> ChanId -> Update NoC (Either Error ())
addUserSubscription uid c = modify $ \ noc -> addUserSubscriptionR noc uid c

rmUserSubscription :: UserId -> ChanId -> Update NoC (Either Error ())
rmUserSubscription uid c = modify $ \ noc -> rmUserSubscriptionR noc uid c

$(makeAcidic ''NoC  [ 'createChan
                    , 'createUser
                    , 'addAdmin
                    , 'rmAdmin 
                    , 'setChanName
                    , 'setChanDesc
                    , 'setChanType
                    , 'setChanImage
                    , 'addChanOwner
                    , 'rmChanOwner
                    , 'addChanProducer
                    , 'rmChanProducer
                    , 'addChanConsumer
                    , 'rmChanConsumer
                    , 'post
                    , 'setUserLogin
                    , 'setUserPassword
                    , 'setUserName
                    , 'setUserDesc
                    , 'setUserIcon
                    , 'addUserNotification
                    , 'addUserContact
                    , 'rmUserContact
                    , 'addUserSubscription
                    , 'rmUserSubscription
                    ])
