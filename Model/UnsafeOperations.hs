module Model.UnsafeOperations
where

import Data.IxSet 

import Model.BaseTypes
import Model.OpMonad
import Model.NoC 
--import qualified Model.NoC as N
import Model.Channel
--import qualified Model.Channel as C
import Model.User 
--import qualified Model.User as U
import Model.Message
--import qualified Model.Message as M

getChannels :: Operation b (IxSet Channel) 
getChannels = Operation $ \ s -> Right (s, _channels . noc $ s)

getChannel :: ChanId -> Operation b (Maybe Channel)
getChannel cid = Operation $ \ s -> Right (s, _channels (noc s) @= cid)

setChannel :: Channel -> Operation b ()
setChannel chan = Operation $ \ s -> 

nextChanId :: Operation b ChanId 

getUsers :: Operation b (IxSet User)
getUser :: UserId -> Operation b (Maybe User)
setUser :: User -> Operation b ()
nextUserId :: Operation b UserId 

getMessages :: Operation b (IxSet Message) 
getMessage :: Message -> Operation b (Maybe Message)
setMessage :: Message -> Operation b ()
nextMsgId :: Operation b MsgId

addAdmin :: UserId -> Operation b ()
rmAdmin :: UserId -> Operation b ()
