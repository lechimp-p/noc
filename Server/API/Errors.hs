{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Errors
where

{--import qualified Data.Text as T
import Happstack.Server 
        ( Response, FilterMonad, ok
        , toResponse, ServerMonad
        , setFilter, getFilter, composeFilter
        , askRq, localRq, badRequest
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
import Control.Monad.Trans.JSON
--}
import qualified Model.Errors as ME
import API.Effects
import API.ImageUtils 

import Control.Eff
import Control.Eff.JSON (JSONError)
import Data.Monoid

data Error
    = ModelError' ME.Error
    | JSONError' JSONError
    | ImageError' ImageError
    deriving (Show)

instance Monoid Error where
    mempty = Abort
    a `mappend` _ = a

{--handleError :: ( Monad m, MonadIO m 
               , FilterMonad Response m 
               )
            => EitherT Error m Response
            -> m Response
handleError op = runEitherT op >>= \ res -> 
    case res of
        Left err -> respondError err
        Right res -> return res
--}


            
respondError :: (Member API r)
             => Error -> Eff r () 
respondError = badRequest . T.pack . show 

{--
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
--}
