{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Errors
where

import qualified Data.Text as T
import Happstack.Server 
        ( Response, FilterMonad, ok
        , toResponse, ServerMonad
        , setFilter, getFilter, composeFilter
        , askRq, localRq, badRequest
        , unauthorized
        )
import Happstack.Server.ClientSession
        ( MonadClientSession, getSession
        , putSession, expireSession
        ) 
import Happstack.Server.Internal.Monads
        ( WebMonad, finishWith )
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Data.Monoid
import Control.Monad.Trans.JSON

import qualified Model.Errors as ME
import Model
import API.ImageUtils 
import API.APIMonad
import API.Config
import ACID

data Error
    = ModelError' ME.Error
    | JSONError' JSONError
    | ImageError' ImageError
    | QueryParamNotFound String
    | QueryParamNotConverted String String
    | Abort
    deriving (Show)

instance Monoid Error where
    mempty = Abort
    a `mappend` _ = a

handleError :: ( Monad m, MonadIO m 
               , FilterMonad Response m 
               )
            => EitherT Error m Response
            -> m Response
handleError op = runEitherT op >>= \ res -> 
    case res of
        Left err -> respondError err
        Right res -> return res

defaultResponse :: (Monad m, MonadIO m, FilterMonad Response m, Show a)
               => a -> m Response
defaultResponse = badRequest . toResponse . T.pack . show 
            
respondError :: (Monad m, MonadIO m, FilterMonad Response m)
             => Error -> m Response
respondError (ModelError' e) = respondModelError e
respondError e = defaultResponse e

respondModelError :: (Monad m, MonadIO m, FilterMonad Response m)
                  => ME.Error -> m Response
respondModelError ME.NotLoggedIn = unauthorized . toResponse . T.pack $ "You are not logged in." 
responeModelError e = defaultResponse e 

throwAPIError e = left e

instance Monad m => MonadJSONError (EitherT Error m) where
    throwJSONError = left . JSONError'

instance Monad m => MonadQueryError (EitherT Error m) where
    throwQueryError = left . ModelError'

instance Monad m => MonadUpdateError (EitherT Error m) where
    throwUpdateError = left . ModelError'

instance Monad m => MonadImageError (EitherT Error m) where
    throwImageError = left . ImageError'

instance WithConfig m => WithConfig (EitherT Error m) where
    config = lift . config

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

instance WebMonad Response m 
      => WebMonad Response (EitherT Error m)
    where
    finishWith = lift . finishWith
