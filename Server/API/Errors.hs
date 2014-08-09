{-# LANGUAGE FlexibleContexts #-}

module API.Errors
where

import Happstack.Server ( Response, FilterMonad)

import Model.Errors
import Model
import API.ACIDEvents
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

respondError :: FilterMonad Response m => Error -> APIMonadT url session m Response
respondError = ok' . show 
