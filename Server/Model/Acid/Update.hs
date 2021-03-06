module Model.Acid.Update
where

import Model.BaseTypes
import Model.Message
import Model.Errors
import Model.Simple.Operations
import Model.Acid.SafeCopy

import Data.Acid
import Data.Text (Text)
import qualified Data.Set as S
import Control.Monad.State (get, put)
import Data.Time.Clock (UTCTime)
import Text.Email.Validate (EmailAddress)

modify :: (NoC -> Either Error (NoC, a)) -> Update NoC (Either Error a)
modify f = do
    noc <- get 
    case f noc of
        Left err -> return . Left $ err
        Right (noc, res) -> put noc >> return (Right res)

uCreateChan :: UserId -> Name -> Update NoC (Either Error ChanId)
uCreateChan uid name = modify $ \ noc -> Right $ createChanR noc uid name

uCreateUser :: Login -> Password -> Update NoC (Either Error UserId)
uCreateUser l pw = modify $ \ noc -> Right $ createUserR noc l pw

uAddAdmin :: UserId -> Update NoC (Either Error ())
uAddAdmin uid = modify $ \ noc -> Right $ addAdminR noc uid

uRmAdmin :: UserId -> Update NoC (Either Error ())
uRmAdmin uid = modify $ \ noc -> Right $ rmAdminR noc uid

uSetChanName :: ChanId -> Name -> Update NoC (Either Error ())
uSetChanName cid n = modify $ \ noc -> setChanNameR noc cid n

uSetChanDesc :: ChanId -> Desc -> Update NoC (Either Error ())
uSetChanDesc cid d = modify $ \ noc -> setChanDescR noc cid d

uSetChanType :: ChanId -> ChanType -> Update NoC (Either Error ())
uSetChanType cid t = modify $ \ noc -> setChanTypeR noc cid t

uSetChanImage :: ChanId -> Maybe Image -> Update NoC (Either Error ())
uSetChanImage cid img = modify $ \ noc -> setChanImageR noc cid img 

uAddChanOwner :: ChanId -> UserId -> Update NoC (Either Error ())
uAddChanOwner cid uid = modify $ \ noc -> addChanOwnerR noc cid uid

uRmChanOwner :: ChanId -> UserId -> Update NoC (Either Error ())
uRmChanOwner cid uid = modify $ \ noc -> rmChanOwnerR noc cid uid

uAddChanProducer :: ChanId -> UserId -> Update NoC (Either Error ())
uAddChanProducer cid uid = modify $ \ noc -> addChanProducerR noc cid uid

uRmChanProducer :: ChanId -> UserId -> Update NoC (Either Error ())
uRmChanProducer cid uid = modify $ \ noc -> rmChanProducerR noc cid uid

uAddChanConsumer :: ChanId -> UserId -> Update NoC (Either Error ())
uAddChanConsumer cid uid = modify $ \ noc -> addChanConsumerR noc cid uid

uRmChanConsumer :: ChanId -> UserId -> Update NoC (Either Error ())
uRmChanConsumer cid uid = modify $ \ noc -> rmChanConsumerR noc cid uid

uPost :: ChanId -> UserId -> UTCTime -> Text -> Maybe Image -> Update NoC (Either Error MsgId)
uPost cid uid ts txt img = modify $ \ noc -> postR noc cid uid ts txt img

uSetUserLogin :: UserId -> Login -> Update NoC (Either Error ())
uSetUserLogin uid l = modify $ \ noc -> setUserLoginR noc uid l

uSetUserPassword :: UserId -> Password -> Update NoC (Either Error ())
uSetUserPassword uid l = modify $ \ noc -> setUserPasswordR noc uid l

uSetUserName :: UserId -> Name -> Update NoC (Either Error ())
uSetUserName uid l = modify $ \ noc -> setUserNameR noc uid l

uSetUserDesc :: UserId -> Desc -> Update NoC (Either Error ())
uSetUserDesc uid l = modify $ \ noc -> setUserDescR noc uid l

uSetUserIcon :: UserId -> Maybe Icon -> Update NoC (Either Error ())
uSetUserIcon uid l = modify $ \ noc -> setUserIconR noc uid l

uSetUserEmail :: UserId -> Maybe EmailAddress -> Update NoC (Either Error ())
uSetUserEmail uid e = modify $ \ noc -> setUserEmailR noc uid e

uAddUserNotification :: UserId -> Notification -> Update NoC (Either Error ())
uAddUserNotification uid n = modify $ \ noc -> addUserNotificationR noc uid n

uSetUserContact :: UserId -> Contact -> Update NoC (Either Error ())
uSetUserContact uid c = modify $ \ noc -> setUserContactR noc uid c

uRmUserContactTo :: UserId -> UserId -> Update NoC (Either Error ())
uRmUserContactTo uid c = modify $ \ noc -> rmUserContactToR noc uid c

uAddUserSubscription :: UserId -> ChanId -> Update NoC (Either Error ())
uAddUserSubscription uid c = modify $ \ noc -> addUserSubscriptionR noc uid c

uRmUserSubscription :: UserId -> ChanId -> Update NoC (Either Error ())
uRmUserSubscription uid c = modify $ \ noc -> rmUserSubscriptionR noc uid c
