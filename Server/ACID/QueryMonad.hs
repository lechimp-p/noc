{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module ACID.QueryMonad
    ( MonadQueryError
    , throwQueryError
    , MonadQuery 
    , doLoginQ
    , getOperatorIdQ
    , getChanNameQ
    , getChanDescQ
    , getUserLoginQ
    , getUserNameQ
    , getUserDescQ
    , getUserIconQ
    , getUserOwnedChannelsQ
    , getUserSubscriptionsQ
    , getUserContactsQ
    , getUserByLoginQ
    , messagesQ
    , QueryMonadT
    , runQueryMonadT
    , runQueryMonadT'
    , setOperatorIdQ
    , getAcidQ
    , maybeOperatorIdQ
    )
where

import Data.Text
import Data.Acid
        ( QueryEvent, EventResult
        , EventState, AcidState
        )
import Data.Acid.Advanced ( query' )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Set as S

import Model
import Model.Errors
import ACID.Acidic

class Monad m => MonadQueryError m where
    throwQueryError :: Error -> m a

class MonadQueryError m => MonadQuery m where
    doLoginQ                :: Login -> Password -> m ()
    getOperatorIdQ          :: m UserId
    getChanNameQ            :: ChanId -> m Name
    getChanDescQ            :: ChanId -> m Desc
    getUserLoginQ           :: UserId -> m Login 
    getUserNameQ            :: UserId -> m Name
    getUserDescQ            :: UserId -> m Desc
    getUserIconQ            :: UserId -> m (Maybe Icon)
    getUserOwnedChannelsQ   :: UserId -> m (S.Set ChanId)
    getUserSubscriptionsQ   :: UserId -> m (S.Set ChanId)
    getUserContactsQ        :: UserId -> m (S.Set UserId)
    getUserByLoginQ         :: Text -> m UserId
    messagesQ               :: ChanId -> Offset -> Amount -> m [Message] 



data QueryMonadT acid (m :: * -> *) a where
    DoQuery :: ( QueryEvent event
               , EventResult event 
               ~ (Either Error a, Maybe UserId)
               ) 
            => (Maybe UserId -> event) 
            -> QueryMonadT (EventState event) m a 

    Return  :: a -> QueryMonadT acid m a

    Bind    :: QueryMonadT acid m a 
            -> (a -> QueryMonadT acid m b) 
            -> QueryMonadT acid m b

    Lift    :: m a
            -> QueryMonadT acid m a

    Plus    :: (MonadPlus m)
            => QueryMonadT acid m a
            -> QueryMonadT acid m a
            -> QueryMonadT acid m a

    -- i don't like these, but seems they are necessary to
    -- implement FilterMonad over this
    SetOperatorId :: Monad m
            => Maybe UserId
            -> QueryMonadT acid m () 

    GetAcid :: Monad m
            => QueryMonadT acid m (AcidState acid) 

    MaybeOperatorId :: Monad m
            => QueryMonadT acid m (Maybe UserId)


instance Monad m => Monad (QueryMonadT acid m) where
    return = Return 
    (>>=) = Bind 

instance MonadTrans (QueryMonadT acid) where
    lift = Lift

instance MonadIO m => MonadIO (QueryMonadT acid m) where
    liftIO = lift . liftIO 

instance MonadPlus m => MonadPlus (QueryMonadT acid m) where
    mzero = lift mzero
    mplus = Plus
    
instance MonadQueryError m => MonadQueryError (QueryMonadT acid m) where
    throwQueryError = lift . throwQueryError

instance MonadQueryError m => MonadQuery (QueryMonadT NoC m) where
    doLoginQ l p = DoQuery $ const $ DoLoginQ l p 
    getOperatorIdQ = DoQuery GetOperatorIdQ
    getChanNameQ c = DoQuery $ \ o -> GetChanNameQ o c
    getChanDescQ c = DoQuery $ \ o -> GetChanDescQ o c
    getUserLoginQ u = DoQuery $ \ o -> GetUserLoginQ o u
    getUserNameQ u = DoQuery $ \ o -> GetUserNameQ o u
    getUserDescQ u = DoQuery $ \ o -> GetUserDescQ o u
    getUserIconQ u = DoQuery $ \ o -> GetUserIconQ o u
    getUserOwnedChannelsQ u = DoQuery $ \ o -> GetUserOwnedChannelsQ o u
    getUserSubscriptionsQ u = DoQuery $ \ o -> GetUserSubscriptionsQ o u
    getUserContactsQ u = DoQuery $ \ o -> GetUserContactsQ o u 
    getUserByLoginQ u = DoQuery $ \ o -> GetUserByLoginQ o u
    messagesQ c o a = DoQuery $ \ u -> MessagesQ u c o a

setOperatorIdQ :: Monad m => Maybe UserId -> QueryMonadT acid m ()
setOperatorIdQ = SetOperatorId
getAcidQ :: Monad m => QueryMonadT acid m (AcidState acid)
getAcidQ = GetAcid
maybeOperatorIdQ :: Monad m => QueryMonadT acid m (Maybe UserId)
maybeOperatorIdQ = MaybeOperatorId


runQueryMonadT :: (MonadQueryError m, MonadIO m ) 
              => QueryMonadT acid m a 
              -> AcidState acid 
              -> m a
runQueryMonadT m acid = runQueryMonadT' m acid Nothing >>= return . fst

runQueryMonadT' :: (MonadQueryError m, MonadIO m) 
               => QueryMonadT acid m a 
               -> AcidState acid 
               -> Maybe UserId 
               -> m (a, Maybe UserId)
runQueryMonadT' m acid uid =
    case m of
        DoQuery fun -> do
            (v, uid') <- query' acid $ fun uid
            case v of
                Left err -> throwQueryError err
                Right v' -> return (v', uid') 
        Return v -> return (v, uid)
        Lift m -> m >>= \ v -> return (v, uid)
        Bind v f -> do
            (v', uid') <- runQueryMonadT' v acid uid
            runQueryMonadT' (f v') acid uid'
        Plus l r -> (runQueryMonadT' l acid uid) `mplus` (runQueryMonadT' r acid uid)
        SetOperatorId uid -> return ((), uid)
        GetAcid -> return (acid, uid)
        MaybeOperatorId -> return (uid, uid)
        
