module Model.UnsafeOperations
where

import Data.IxSet 
import qualified Data.Set as S
import Control.Lens

import Model.BaseTypes
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

getChannel :: ChanId -> Operation (Maybe Channel)
getChannel cid = Operation $ \ s -> Right (s, getOne $ (s ^. noc . channels) @= cid)

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

getUser :: UserId -> Operation (Maybe User)
getUser uid = Operation $ \ s -> Right (s, getOne $ (s ^. noc . users) @= uid)

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

getMessage :: MsgId -> Operation (Maybe Message)
getMessage mid = Operation $ \ s -> Right (s, getOne $ (s ^. noc . N.messages) @= mid)

storeMessage :: Message -> Operation ()
storeMessage msg = Operation $ \ s -> 
    let s' = over (noc . N.messages) (insert msg . deleteIx (M._id msg)) s
    in Right (s', ())

newMsgId :: Operation MsgId
newMsgId = Operation $ \ s ->
    let mid = s ^. noc . nextMsgId
        s' = set (noc . nextMsgId) (MsgId (1 + miToInt mid)) s
    in Right (s', mid)

addAdmin :: UserId -> Operation ()
addAdmin uid = Operation $ \ s ->
    let s' = over (noc . admins) (S.insert uid) s
    in Right (s', ())

rmAdmin :: UserId -> Operation ()
rmAdmin uid = Operation $ \ s ->
    let s' = over (noc . admins) (S.delete uid) s
    in Right (s', ())
