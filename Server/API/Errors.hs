{-# LANGUAGE FlexibleContexts #-}

module API.Errors
where

import Happstack.Server ( Response, FilterMonad)
import Control.Monad.IO.Class

import Model.Errors
import Model
import API.Utils
import API.Monad

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

handleError :: (Monad m, MonadIO m)
            => APIMonadT url session m (Either Error Response)
            -> APIMonadT url session m Response
handleError op = op >>= \res -> 
    case res of
        Left err -> respondError err
        Right res -> return res
            
respondError :: (Monad m, MonadIO m)
             => Error -> APIMonadT url session m Response
respondError = ok' . show 
