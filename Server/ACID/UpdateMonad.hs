{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module ACID.UpdateMonad
    ( MonadUpdateError
    , throwUpdateError
    , MonadUpdate
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
    , subscribeToChanU
    , unsubscribeFromChanU
    , getUserLoginU
    , getUserNameU
    , getUserDescU
    , getUserIconU
    , setUserLoginU
    , setUserPasswordU
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
    , UpdateMonadT
    , runUpdateMonadT
    , runUpdateMonadT'
    , setOperatorIdU
    , getAcidU
    , maybeOperatorIdU
    )
where

import Data.Text
import Data.Acid
        ( UpdateEvent, EventResult
        , EventState, AcidState
        )
import Data.Acid.Advanced ( update' )
import Data.Time.Clock
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Set as S

import Model
import Model.Errors
import ACID.Acidic

class Monad m => MonadUpdateError m where
    throwUpdateError :: Error -> m a

class MonadUpdateError m => MonadUpdate m where
    doLoginU            :: Login -> Password -> m ()
    getOperatorIdU      :: m UserId 
    addAdminU           :: UserId -> m () 
    rmAdminU            :: UserId -> m () 
    getChanNameU        :: ChanId -> m Name 
    getChanDescU        :: ChanId -> m Desc 
    setChanNameU        :: ChanId -> Name -> m () 
    setChanDescU        :: ChanId -> Desc -> m ()
    addChanOwnerU       :: ChanId -> UserId -> m ()
    addChanProducerU    :: ChanId -> UserId -> m ()
    addChanConsumerU    :: ChanId -> UserId -> m ()
    rmChanOwnerU        :: ChanId -> UserId -> m ()
    rmChanProducerU     :: ChanId -> UserId -> m ()
    rmChanConsumerU     :: ChanId -> UserId -> m ()
    subscribeToChanU    :: UserId -> ChanId -> m ()
    unsubscribeFromChanU:: UserId -> ChanId -> m ()
    getUserLoginU       :: UserId -> m Login
    getUserNameU        :: UserId -> m Name
    getUserDescU        :: UserId -> m Desc
    getUserIconU        :: UserId -> m (Maybe Icon)
    setUserLoginU       :: UserId -> Login -> m ()
    setUserPasswordU    :: UserId -> Password -> m ()
    setUserNameU        :: UserId -> Name -> m ()
    setUserDescU        :: UserId -> Desc -> m ()
    setUserIconU        :: UserId -> Maybe Icon -> m ()
    getUserOwnedChannelsU :: UserId -> m (S.Set ChanId)
    getUserSubscriptionsU :: UserId -> m (S.Set ChanId) 
    getUserContactsU    :: UserId -> m (S.Set UserId) 
    getUserByLoginU     :: Text -> m UserId
    createUserU         :: Login -> Password -> m UserId
    createChannelU      :: Name -> Desc -> m ChanId
    postU               :: ChanId -> UTCTime -> Text -> Maybe Image -> m MsgId
    messagesU           :: ChanId -> Offset -> Amount -> m [Message]



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

    Plus    :: (MonadPlus m)
            => UpdateMonadT acid m a
            -> UpdateMonadT acid m a
            -> UpdateMonadT acid m a

    -- i don't like these, but seems they are necessary to
    -- implement FilterMonad over this
    SetOperatorId :: Monad m
            => Maybe UserId
            -> UpdateMonadT acid m () 

    GetAcid :: Monad m
            => UpdateMonadT acid m (AcidState acid) 

    MaybeOperatorId :: Monad m
            => UpdateMonadT acid m (Maybe UserId)



instance Monad m => Monad (UpdateMonadT acid m) where
    return = Return 
    (>>=) = Bind 

instance MonadTrans (UpdateMonadT acid) where
    lift = Lift

instance MonadIO m => MonadIO (UpdateMonadT acid m) where
    liftIO = lift . liftIO

instance MonadPlus m => MonadPlus (UpdateMonadT acid m) where
    mzero = lift mzero
    mplus = Plus

instance MonadUpdateError m => MonadUpdateError (UpdateMonadT acid m) where
    throwUpdateError = lift . throwUpdateError

instance MonadUpdateError m => MonadUpdate (UpdateMonadT NoC m) where
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
    subscribeToChanU u c = DoUpdate $ \ o -> SubscribeToChanU o u c
    unsubscribeFromChanU u c = DoUpdate $ \ o -> UnsubscribeFromChanU o u c
    getUserLoginU u = DoUpdate $ \ o -> GetUserLoginU o u
    getUserNameU u = DoUpdate $ \ o -> GetUserNameU o u
    getUserDescU u = DoUpdate $ \ o -> GetUserDescU o u
    getUserIconU u = DoUpdate $ \ o -> GetUserIconU o u
    setUserLoginU u l = DoUpdate $ \ o -> SetUserLoginU o u l
    setUserPasswordU u l = DoUpdate $ \ o -> SetUserPasswordU o u l
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

setOperatorIdU :: Monad m => Maybe UserId -> UpdateMonadT acid m ()
setOperatorIdU = SetOperatorId
getAcidU :: Monad m => UpdateMonadT acid m (AcidState acid)
getAcidU = GetAcid
maybeOperatorIdU :: Monad m => UpdateMonadT acid m (Maybe UserId)
maybeOperatorIdU = MaybeOperatorId


runUpdateMonadT :: ( MonadUpdateError m, MonadIO m ) 
              => UpdateMonadT acid m a 
              -> AcidState acid 
              -> m a
runUpdateMonadT m acid = runUpdateMonadT' m acid Nothing >>= return . fst

runUpdateMonadT' :: ( MonadUpdateError m, MonadIO m ) 
               => UpdateMonadT acid m a 
               -> AcidState acid 
               -> Maybe UserId 
               -> m (a, Maybe UserId)
runUpdateMonadT' m acid uid =
    case m of
        DoUpdate fun -> do
            (v, uid') <- update' acid $ fun uid
            case v of
                Left err -> throwUpdateError err
                Right v' -> return (v', uid')
        Return v -> return (v, uid)
        Lift m -> m >>= \ v -> return (v, uid)
        Bind v f -> do
            (v', uid') <- runUpdateMonadT' v acid uid
            runUpdateMonadT' (f v') acid uid'
        Plus l r -> (runUpdateMonadT' l acid uid) `mplus` (runUpdateMonadT' r acid uid)
        SetOperatorId uid -> return ((), uid)
        GetAcid -> return (acid, uid)
        MaybeOperatorId -> return (uid, uid)
