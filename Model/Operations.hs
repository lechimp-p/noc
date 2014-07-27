{-# LANGUAGE RankNTypes #-}

module Model.Operations
    ( getChanName
    , getChanDesc
    , setChanName
    , setChanDesc
    , addChanOwner
    , addChanProducer
    , addChanConsumer
    , rmChanOwner
    , rmChanProducer
    , rmChanConsumer
    )
where

import qualified Data.Set as S
import Control.Lens

import Model.UnsafeOperations
import Model.Errors
import Model.Permissions
import Model.BaseTypes
import Model.OpMonad
import qualified Model.UnsafeOperations as US
import qualified Model.Channel as C
import Model.Channel
import qualified Model.User as U
import Model.User 
import qualified Model.Message as M
import Model.Message 

type SimpleLens a b = Lens a a b b 

-- operations on channels

getFromChan :: SimpleLens Channel a -> ChanId -> Operation a
getFromChan l cid = checkAccess cid forAllChanPeople $ do
    chan <- US.getChannel cid
    return $ chan ^. l

setToChan :: SimpleLens Channel a -> ChanId -> a -> Operation ()
setToChan l cid v = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    US.storeChannel (set l v chan)  

getChanName = getFromChan C.name
getChanDesc = getFromChan C.desc

setChanName = setToChan C.name
setChanDesc = setToChan C.desc


addChanXX :: SimpleLens Channel (S.Set UserId)
          -> ChanId -> UserId -> Operation ()
addChanXX l cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    US.storeChannel (over l (\ s -> uid `S.insert` s) chan)

rmChanXX :: SimpleLens Channel (S.Set UserId)
         -> ChanId -> UserId -> Operation ()
rmChanXX l cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    US.storeChannel (over l (\ s -> uid `S.delete` s) chan)

addChanOwner = addChanXX C.owners
addChanProducer = addChanXX C.producers
addChanConsumer = addChanXX C.consumers

rmChanOwner cid uid = checkAccess cid forChanOwnersOrAdmins $ do
    chan <- US.getChannel cid
    OnlyOneChannelOwnerLeft `throwOn` (size (chan ^. owners) == 0)
    US.storeChannel (over l (\ s -> uid `S.delete` s) chan)
rmChanProducer = rmChanXX C.producers
rmChanConsumer = rmChanXX C.consumers


