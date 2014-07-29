module Model.UnsafeOperations
where

import Data.Data (Data, Typeable)
import Data.IxSet hiding ((@=))
import qualified Data.IxSet as IX
import qualified Data.Set as S
import Control.Lens

import Model.BaseTypes
import Model.Errors
import Model.OpMonad
import Model.NoC 
import qualified Model.NoC as N
import Model.Channel
import qualified Model.Channel as C
import Model.User 
import qualified Model.User as U
import Model.Message
import qualified Model.Message as M

getChannels :: Operation (IxSet Channel) 
getChannels = Operation $ \ s -> Right (s, s ^. noc . channels)

getChannelMaybe :: ChanId -> Operation (Maybe Channel)
getChannelMaybe cid = Operation $ \ s -> Right (s, getOne $ (s ^. noc . channels) IX.@= cid)

getChannel :: ChanId -> Operation Channel
getChannel cid = getChannelMaybe cid >>= \chan ->
    case chan of
        (Just chan') -> return chan'
        Nothing -> throw $ UnknownChannel cid 

storeChannel :: Channel -> Operation ()
storeChannel chan = Operation $ \ s -> 
    let s' = over (noc . channels) (insert chan . deleteIx (C._id chan)) s
    in Right (s', ())

newChanId :: Operation ChanId 
newChanId = Operation $ \ s ->
    let cid = s ^. noc . nextChanId
        s' = set (noc . nextChanId) (ChanId (1 + ciToInt cid)) s
    in Right (s', cid)

getUsers :: Operation (IxSet User)
getUsers = Operation $ \ s -> Right (s, s ^. noc . users)

getUserMaybe :: UserId -> Operation (Maybe User)
getUserMaybe uid = Operation $ \ s -> Right (s, getOne $ (s ^. noc . users) IX.@= uid)

getUser :: UserId -> Operation User
getUser uid = getUserMaybe uid >>= \ user ->
    case user of
        (Just user') -> return user'
        Nothing -> throw $ UnknownUser uid

storeUser :: User -> Operation ()
storeUser user = Operation $ \ s -> 
    let s' = over (noc . users ) (insert user . deleteIx (U._id user)) s
    in Right (s', ())

newUserId :: Operation UserId 
newUserId = Operation $ \ s ->
    let uid = s ^. noc . nextUserId
        s' = set (noc . nextUserId) (UserId (1 + uiToInt uid)) s
    in Right (s', uid)

getMessages :: Operation (IxSet Message) 
getMessages = Operation $ \ s -> Right (s, s ^. noc . N.messages)

getMessageMaybe :: MsgId -> Operation (Maybe Message)
getMessageMaybe mid = Operation $ \ s -> Right (s, getOne $ (s ^. noc . N.messages) IX.@= mid)

getMessage :: MsgId -> Operation Message
getMessage mid = getMessageMaybe mid >>= \ msg ->
    case msg of
        (Just msg') -> return msg'
        Nothing -> throw $ UnknownMessage mid

storeMessage :: Message -> Operation ()
storeMessage msg = Operation $ \ s -> 
    let s' = over (noc . N.messages) (insert msg . deleteIx (M._id msg)) s
    in Right (s', ())

newMsgId :: Operation MsgId
newMsgId = Operation $ \ s ->
    let mid = s ^. noc . nextMsgId
        s' = set (noc . nextMsgId) (MsgId (1 + miToInt mid)) s
    in Right (s', mid)

getAdmins :: Operation (S.Set UserId)
getAdmins = Operation $ \ s -> Right (s, s ^. noc . admins)

addAdmin :: UserId -> Operation ()
addAdmin uid = Operation $ \ s ->
    let s' = over (noc . admins) (S.insert uid) s
    in Right (s', ())

rmAdmin :: UserId -> Operation ()
rmAdmin uid = Operation $ \ s ->
    let s' = over (noc . admins) (S.delete uid) s
    in Right (s', ())

getOperatorId :: Operation UserId
getOperatorId = Operation $ \ s -> Right (s, _operator s)

infixl 9 @=

(@=) :: (IX.Indexable a, Typeable a, Ord a, Typeable k)
      => Operation (IxSet a) -> k -> Operation (IxSet a)
op @= k = fmap (IX.@= k) op
