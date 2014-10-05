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
import API.ImageUtils (ImageError)
import API.Session (AuthData)
import API.Config (APIConfig)

import Control.Eff
import Control.Eff.JSON (JSONError)
import Data.Monoid
import qualified Data.Text as T

data Error
    = ModelError' ME.Error
    | JSONError' JSONError
    | ImageError' ImageError
    | JustStopped
    deriving (Show)

class IsError err where
    toError :: err -> Error

instance Monoid Error where
    mempty = JustStopped 
    a `mappend` _ = a

instance IsError ME.Error where
    toError = ModelError'

instance IsError JSONError where
    toError = JSONError'

instance IsError ImageError where
    toError = ImageError'

handleError :: (Member API r, IsError err)
            => Eff r (Either err a) -> Eff r a
handleError eff = do
    res <- eff
    case res of
        Left err -> respondError (toError err) >> abort >> return undef
        Right v -> return v

    where
    undef = error "API/Errors::handleError: do not evaluate result after call to abort."

respondError :: Member API r
             => Error -> Eff r () 
respondError = badRequest . T.pack . show 
