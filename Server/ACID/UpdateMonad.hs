{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ACID.UpdateMonad
    ( UpdateMonadT
    , runUpdateMonadT
    , runUpdateMonadT'
    , doLoginU
    , getOperatorIdU
    , addAdminU
    , rmAdminU
    , getChanNameU
    , getChanDescU
    , setChanNameU
    , setChanDescU
    , addChanOwnerU
    , addChanProducerU
    , addChanConsumerU
    , rmChanOwnerU
    , rmChanProducerU
    , rmChanConsumerU
    , getUserLoginU
    , getUserNameU
    , getUserDescU
    , getUserIconU
    , setUserLoginU
    , setUserNameU
    , setUserDescU
    , setUserIconU
    , getUserOwnedChannelsU
    , getUserSubscriptionsU
    , getUserContactsU
    , getUserByLoginU
    , messagesU
    , createUserU
    , createChannelU
    , postU
    )
where

import Data.Acid
        ( UpdateEvent, EventResult
        , EventState, AcidState
        )
import Data.Acid.Advanced ( update' )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Model
import Model.Errors
import ACID.Acidic

data UpdateMonadT acid (m :: * -> *) a where
    DoUpdate :: ( UpdateEvent event
               , EventResult event 
               ~ (Either Error a, Maybe UserId)
               ) 
            => (Maybe UserId -> event) 
            -> UpdateMonadT (EventState event) m a 

    Return  :: a -> UpdateMonadT acid m a

    Bind    :: UpdateMonadT acid m a 
            -> (a -> UpdateMonadT acid m b) 
            -> UpdateMonadT acid m b

    Lift    :: m a
            -> UpdateMonadT acid m a


instance Monad m => Monad (UpdateMonadT acid m) where
    return = Return 
    (>>=) = Bind 

instance MonadTrans (UpdateMonadT acid) where
    lift = Lift

runUpdateMonadT :: ( MonadIO m ) 
              => UpdateMonadT acid m a 
              -> AcidState acid 
              -> m (Either Error a)
runUpdateMonadT m acid = runUpdateMonadT' m acid Nothing >>= return . fst

runUpdateMonadT' :: ( MonadIO m ) 
               => UpdateMonadT acid m a 
               -> AcidState acid 
               -> Maybe UserId 
               -> m (Either Error a, Maybe UserId)
runUpdateMonadT' m acid uid =
    case m of
        DoUpdate fun -> update' acid $ fun uid
        Return v -> return (Right v, uid)
        Lift m -> m >>= \ v -> return (Right v, uid)
        Bind v f -> do
            v' <- runUpdateMonadT' v acid uid
            case v' of
                (Left err, uid') -> return (Left err, uid') 
                (Right res, uid') -> runUpdateMonadT' (f res) acid uid'
            

doLoginU l p = DoUpdate $ const $ DoLoginU l p 
getOperatorIdU = DoUpdate GetOperatorIdU
addAdminU u = DoUpdate $ \ o -> AddAdminU o u
rmAdminU u = DoUpdate $ \ o -> RmAdminU o u
getChanNameU c = DoUpdate $ \ o -> GetChanNameU o c
getChanDescU c = DoUpdate $ \ o -> GetChanDescU o c
setChanNameU c n = DoUpdate $ \ o -> SetChanNameU o c n
setChanDescU c d = DoUpdate $ \ o -> SetChanDescU o c d
addChanOwnerU c u = DoUpdate $ \ o -> AddChanOwnerU o c u
addChanProducerU c u = DoUpdate $ \ o -> AddChanProducerU o c u
addChanConsumerU c u = DoUpdate $ \ o -> AddChanConsumerU o c u
rmChanOwnerU c u = DoUpdate $ \ o -> RmChanOwnerU o c u
rmChanProducerU c u = DoUpdate $ \ o -> RmChanProducerU o c u
rmChanConsumerU c u = DoUpdate $ \ o -> RmChanConsumerU o c u
getUserLoginU u = DoUpdate $ \ o -> GetUserLoginU o u
getUserNameU u = DoUpdate $ \ o -> GetUserNameU o u
getUserDescU u = DoUpdate $ \ o -> GetUserDescU o u
getUserIconU u = DoUpdate $ \ o -> GetUserIconU o u
setUserLoginU u l = DoUpdate $ \ o -> SetUserLoginU o u l
setUserNameU u n = DoUpdate $ \ o -> SetUserNameU o u n
setUserDescU u d = DoUpdate $ \ o -> SetUserDescU o u d
setUserIconU u i = DoUpdate $ \ o -> SetUserIconU o u i
getUserOwnedChannelsU u = DoUpdate $ \ o -> GetUserOwnedChannelsU o u
getUserSubscriptionsU u = DoUpdate $ \ o -> GetUserSubscriptionsU o u
getUserContactsU u = DoUpdate $ \ o -> GetUserContactsU o u 
getUserByLoginU u = DoUpdate $ \ o -> GetUserByLoginU o u
createUserU l p = DoUpdate $ \ o -> CreateUserU o l p
createChannelU n d = DoUpdate $ \ o -> CreateChannelU o n d
postU c ts t i = DoUpdate $ \ o -> PostU o c ts t i
messagesU c o a = DoUpdate $ \ u -> MessagesU u c o a
