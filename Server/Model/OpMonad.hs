module Model.OpMonad
where

import qualified Data.IxSet as IX 
import Data.Set ( Set ) 
import Data.Data (Data, Typeable)
import Control.Applicative
import Control.Monad

import Model.Errors ( Error (UnknownUser, UnknownChannel, UnknownMessage ) )
import Model.BaseTypes
import Model.Channel
import Model.User
import Model.Message

class (Functor m, Applicative m, Monad m) => OpMonad m where
    throw :: Error -> m a
    getChannels :: m (IX.IxSet Channel)
    storeChannel :: Channel -> m ()
    newChanId :: m ChanId
    getUsers :: m (IX.IxSet User)
    storeUser :: User -> m ()
    newUserId :: m UserId
    getMessages :: m (IX.IxSet Message)
    storeMessage :: Message -> m () 
    newMsgId :: m MsgId
    getAdmins :: m (Set UserId)
    addAdmin :: UserId -> m ()
    rmAdmin :: UserId -> m ()
    getOperatorId :: m (Maybe UserId)
    doLogin :: Login -> Password -> m ()
    doLogout :: m ()



infixl 9 @=

(@=) :: (OpMonad m, IX.Indexable a, Typeable a, Ord a, Typeable k)
     => m (IX.IxSet a) -> k -> m (IX.IxSet a)
op @= k = fmap (IX.@= k) op



throwOn :: OpMonad m => Error -> Bool -> m ()
throwOn e c = if c then throw e else return ()

getChannelMaybe :: OpMonad m => ChanId -> m (Maybe Channel)
getChannelMaybe cid = do
    chans <- getChannels 
    return . IX.getOne $ chans IX.@= cid

getChannel :: OpMonad m => ChanId -> m Channel
getChannel cid = getChannelMaybe cid >>= \chan ->
    case chan of
        (Just chan') -> return chan'
        Nothing -> throw $ UnknownChannel cid 

getUserMaybe :: OpMonad m => UserId -> m (Maybe User)
getUserMaybe uid = do
    users <- getUsers
    return . IX.getOne $ users IX.@= uid

getUser :: OpMonad m => UserId -> m User
getUser uid = getUserMaybe uid >>= \ user ->
    case user of
        (Just user') -> return user'
        Nothing -> throw $ UnknownUser uid

getMessageMaybe :: OpMonad m => MsgId -> m (Maybe Message)
getMessageMaybe mid = do
    messages <- getMessages 
    return . IX.getOne $ messages IX.@= mid

getMessage :: OpMonad m => MsgId -> m Message
getMessage mid = getMessageMaybe mid >>= \ msg ->
    case msg of
        (Just msg') -> return msg'
        Nothing -> throw $ UnknownMessage mid

