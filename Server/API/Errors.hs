{-# LANGUAGE FlexibleContexts #-}

module API.Errors
where

import qualified Data.Text as T
import Happstack.Server 
        ( Response, FilterMonad, ok
        , toResponse
        )
import Control.Monad.IO.Class

import qualified Model.Errors as ME
import Model
import API.JSONUtils 
import API.APIMonad

{--handleErrors :: 
             => ACID 
             -> Transaction (Either Error a) NoC 
             -> (a -> APIMonad url session Response) 
             -> APIMonadT url session m Response  
handleErrors acid ta op = do
    res <- getResult acid ta
    case res of
        Right v  -> op v
        Left err -> respondError err
--}

data Error
    = ModelError ME.Error
    | JSONError JSONError'
    deriving (Show)

handleError :: (Monad m, MonadIO m)
            => APIMonadT url session m (Either Error Response)
            -> APIMonadT url session m Response
handleError op = op >>= \res -> 
    case res of
        Left err -> respondError err
        Right res -> return res
            
respondError :: (Monad m, MonadIO m)
             => Error -> APIMonadT url session m Response
respondError = ok . toResponse . T.pack . show 
