{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ACID.QueryMonad
    ( QueryMonadT
    , runQueryMonadT
    , runQueryMonadT'
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
    )
where

import Data.Acid
        ( QueryEvent, EventResult
        , EventState, AcidState
        )
import Data.Acid.Advanced ( query' )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Model
import Model.Errors
import ACID.Acidic

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


instance Monad m => Monad (QueryMonadT acid m) where
    return = Return 
    (>>=) = Bind 

instance MonadTrans (QueryMonadT acid) where
    lift = Lift

runQueryMonadT :: ( MonadIO m ) 
              => QueryMonadT acid m a 
              -> AcidState acid 
              -> m (Either Error a)
runQueryMonadT m acid = runQueryMonadT' m acid Nothing >>= return . fst

runQueryMonadT' :: ( MonadIO m ) 
               => QueryMonadT acid m a 
               -> AcidState acid 
               -> Maybe UserId 
               -> m (Either Error a, Maybe UserId)
runQueryMonadT' m acid uid =
    case m of
        DoQuery fun -> query' acid $ fun uid
        Return v -> return (Right v, uid)
        Lift m -> m >>= \ v -> return (Right v, uid)
        Bind v f -> do
            v' <- runQueryMonadT' v acid uid
            case v' of
                (Left err, uid') -> return (Left err, uid') 
                (Right res, uid') -> runQueryMonadT' (f res) acid uid'
            

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

