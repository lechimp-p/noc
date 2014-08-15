{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Errors
where

import qualified Data.Text as T
import Happstack.Server 
        ( Response, FilterMonad, ok
        , toResponse, ServerMonad
        , setFilter, getFilter, composeFilter
        , askRq, localRq
        )
import Happstack.Server.ClientSession
        ( MonadClientSession, getSession
        , putSession, expireSession
        ) 
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans

import qualified Model.Errors as ME
import Model
import API.JSONUtils 
import API.APIMonad

data Error
    = ModelError' ME.Error
    | JSONError' JSONError
    deriving (Show)

handleError :: (Monad m, MonadIO m)
            => EitherT Error (APIMonadT url session m) Response
            -> APIMonadT url session m Response
handleError op = runEitherT op >>= \res -> 
    case res of
        Left err -> respondError err
        Right res -> return res
            
respondError :: (Monad m, MonadIO m)
             => Error -> APIMonadT url session m Response
respondError = ok . toResponse . T.pack . show 

instance Monad m => MonadJSONError (EitherT Error m) where
    throwJSONError = left . JSONError'

instance FilterMonad Response m 
      => FilterMonad Response (EitherT Error m) 
    where
    setFilter = lift . setFilter
    composeFilter = lift . composeFilter
    -- getFilter :: EitherT Error m a -> EitherT Error m (a, b -> b)
    getFilter m = do
        (v, f) <- lift . getFilter . runEitherT $ m
        case v of
            Left err -> left err
            Right v' -> right (v', f) 

instance ServerMonad m
      => ServerMonad (EitherT Error m)
    where
    askRq = lift askRq
    -- localRq :: (Request -> Request) -> EitherT Error m a -> EitherT Error m a
    localRq f m = do
        v <- lift . localRq f . runEitherT $ m
        case v of
            Left err -> left err
            Right v' -> right v'
