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

import qualified Data.Set as S 
import qualified Data.IxSet as IX
import Data.Time.Clock (UTCTime)

isAdminR noc uid = S.member uid . _admins $ noc
countAdminsR noc = S.size . _admins $ noc
getUserIdByLoginR noc l = fmap U._id . IX.getOne $ _users noc IX.@= (Login l) 
 
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
 

    
head' []     = Nothing
head' (x:_)  = Just x
