{-# LANGUAGE GADTs #-}

module ACID.QueryMonad
    ( runQueryMonad
    , runQueryMonad'
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
    )
where

import Data.Acid
        ( QueryEvent, EventResult
        , EventState, AcidState
        )
import Data.Acid.Advanced ( query' )
import Control.Monad.IO.Class

import Model
import Model.Errors
import ACID.Acidic

data QueryMonad acid a where
    DoQuery :: ( QueryEvent event
               , EventResult event 
               ~ (Either Error a, Maybe UserId)
               ) 
            => (Maybe UserId -> event) 
            -> QueryMonad (EventState event) a 

    Return  :: a -> QueryMonad acid a

    Bind    :: QueryMonad acid a 
            -> (a -> QueryMonad acid b) 
            -> QueryMonad acid b


instance Monad (QueryMonad acid) where
    return = Return 
    (>>=) = Bind 

runOpQueryMonad m acid = runOpQueryMonad m acid Nothing

runQueryMonad' :: ( MonadIO m ) 
              => QueryMonad acid a 
              -> AcidState acid 
              -> Maybe UserId 
              -> m (Either Error a, Maybe UserId)
runQueryMonad' m acid uid =
    case m of
        DoQuery fun -> query' acid $ fun uid
        Return v -> return (Right v, uid)
        Bind v f -> do
            v' <- runQueryMonad v acid uid
            case v' of
                (Left err, uid') -> return (Left err, uid') 
                (Right res, uid') -> runQueryMonad (f res) acid uid'
            

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

