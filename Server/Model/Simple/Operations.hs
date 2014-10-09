{-# LANGUAGE OverloadedStrings #-}

module Model.Simple.Operations where

import Model.Errors
import Model.BaseTypes

import qualified Model.Simple.NoC as N
import Model.Simple.NoC
import qualified Model.Simple.User as U
import Model.Simple.User
import qualified Model.Simple.Channel as C
import Model.Simple.Channel
import qualified Model.Simple.Message as M
import Model.Simple.Message

import Control.Lens 
import qualified Data.Set as S 
import qualified Data.IxSet as IX
import Data.Time.Clock (UTCTime)

doLoginR noc l pw = do
    user <- IX.getOne (_users noc IX.@= l)
    if checkPassword pw (U._password user)
        then return (U._id user)
        else fail "password mismatch"

isAdminR noc uid = S.member uid . _admins $ noc
countAdminsR noc = S.size . _admins $ noc
getUserIdByLoginR noc l = fmap U._id . IX.getOne $ _users noc IX.@= (Login l) 

searchUserByLoginR noc l = S.map U._id . IX.toSet $ _users noc IX.@= (IxLoginSearch l)
searchChanByNameR noc n = S.map C._id . IX.toSet $ _channels noc IX.@= (IxChanNameSearch n)
 
queryChan :: (Channel -> b)
          -> NoC 
          -> ChanId
          -> Either Error b 
queryChan fun noc cid = 
    let chan = IX.getOne (_channels noc IX.@= cid)
    in case chan of
        Nothing -> Left $ UnknownChannel cid
        Just c -> Right $ fun c

getChanNameR = queryChan C._name 
getChanDescR = queryChan C._desc 
getChanTypeR = queryChan C._type'
getChanImageR = queryChan C._image 
isChanOwnerR n c u = queryChan (S.member u . C._owners) n c
isChanProducerR n c u = queryChan (S.member u . C._producers) n c 
isChanConsumerR n c u = queryChan (S.member u . C._consumers) n c
amountOfSubscribedUsersR n c = queryChan (S.size . C._subscribers) n c
lastPostTimestampR n c = queryChan (lastPostTimestamp' c) n c 
    where
    lastPostTimestamp' cid _ = do
        msg <- head' . IX.toDescList (IX.Proxy :: IX.Proxy UTCTime) $ N._messages n IX.@= cid
        return $ _timestamp msg
messagesR n c ofs am = queryChan (messages' ofs am c) n c 
    where
    messages' ofs am cid _ = 
        take am . drop ofs 
        . IX.toDescList (IX.Proxy :: IX.Proxy UTCTime) $ N._messages n IX.@= cid
messagesTillR n c ts = queryChan (messagesTill' ts c) n c
    where
    messagesTill' ts cid _ = 
        takeWhile ((<=) ts . _timestamp) 
        . IX.toDescList (IX.Proxy :: IX.Proxy UTCTime) $ N._messages n IX.@= cid
 

queryUser :: (User -> b)
          -> NoC 
          -> UserId
          -> Either Error b 
queryUser fun noc uid = 
    let user = IX.getOne (_users noc IX.@= uid)
    in case user of
        Nothing -> Left $ UnknownUser uid
        Just u -> Right $ fun u

getUserLoginR = queryUser U._login
getUserNameR = queryUser U._name
getUserDescR = queryUser U._desc
getUserIconR = queryUser U._icon
getUserNotificationsR = queryUser U._notifications
getUserContactsR = queryUser U._contacts
getUserSubscriptionsR = queryUser U._subscriptions


createChanR noc uid name = (noc', cid)
    where
    noc' = over channels (IX.insert chan) (set nextChanId ncid noc)
    chan = Channel cid name (Desc "") None Nothing (S.insert uid S.empty) S.empty S.empty S.empty S.empty  
    cid = _nextChanId noc
    ncid = ChanId(ciToInt cid + 1) 
createUserR noc l pw = (noc', uid)
    where
    noc' = over users (IX.insert user) (set nextUserId nuid noc)
    user = User uid l pw (Name "") (Desc "") Nothing S.empty S.empty S.empty []
    uid  = _nextUserId noc
    nuid = UserId (uiToInt uid + 1)
addAdminR noc uid = (over admins (S.insert uid) noc, ()) 
rmAdminR noc uid = (over admins (S.delete uid) noc, ()) 
 
     
updateChan :: (Channel -> Channel)
           -> a
           -> NoC
           -> ChanId
           -> Either Error (NoC, a)
updateChan mod v noc cid = 
    let chan = IX.getOne (_channels noc IX.@= cid)
    in case chan of
        Nothing -> Left $ UnknownChannel cid
        Just c -> Right (over channels (IX.updateIx cid (mod c)) noc, v)

setChanNameR n c n' = updateChan (set C.name n') () n c
setChanDescR n c d = updateChan (set C.desc d) () n c
setChanTypeR n c t = updateChan (set C.type' t) () n c
setChanImageR n c img = updateChan (set C.image img) () n c
addChanOwnerR n c uid = updateChan (over C.owners (S.insert uid)) () n c
rmChanOwnerR n c uid = updateChan (over C.owners (S.delete uid)) () n c
addChanProducerR n c uid = updateChan (over C.producers (S.insert uid)) () n c
rmChanProducerR n c uid = updateChan (over C.producers (S.delete uid)) () n c
addChanConsumerR n c uid = updateChan (over C.consumers (S.insert uid)) () n c
rmChanConsumerR n c uid = updateChan (over C.consumers (S.delete uid)) () n c
postR n c uid ts txt img = updateChan (over C.messages (S.insert mid)) mid noc' c
    where
    noc' = over N.messages (IX.insert msg) (set nextMsgId nmid n)
    msg = Message mid c img txt uid ts
    mid = N._nextMsgId n
    nmid = MsgId (miToInt mid + 1)
 

updateUser :: (User -> User)
           -> a
           -> NoC
           -> UserId
           -> Either Error (NoC, a)
updateUser mod v noc uid =
    let user = IX.getOne (_users noc IX.@= uid)
    in case user of
        Nothing -> Left $ UnknownUser uid
        Just u -> Right (over users (IX.updateIx uid (mod u)) noc, v)

setUserLoginR n u l = updateUser (set login l) () n u
setUserPasswordR n u pw = updateUser (set password pw) () n u
setUserNameR n u n' = updateUser (set U.name n') () n u
setUserDescR n u d = updateUser (set U.desc d) () n u
setUserIconR n u i = updateUser (set icon i) () n u
addUserNotificationR n u n' = updateUser (over notifications ((:) n')) () n u
addUserContactR n u uid = updateUser (over contacts (S.insert uid)) () n u
rmUserContactR n u uid = updateUser (over contacts (S.delete uid)) () n u
addUserSubscriptionR n u cid = updateUser (over subscriptions (S.insert cid)) () n u
rmUserSubscriptionR n u cid = updateUser (over subscriptions (S.delete cid)) () n u

    
head' []     = Nothing
head' (x:_)  = Just x
