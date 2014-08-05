module API.Errors
where

import Happstack.Server ( Response )

import Model.Errors
import Model
import API.ACIDEvents
import API.Utils
import API.Monad

handleErrors :: ACID 
             -> Transaction (Either Error a) NoC 
             -> (a -> APIMonad url session Response) 
             -> APIMonad url session Response  
handleErrors acid ta op = do
    res <- getResult acid ta
    case res of
        Right v  -> op v
        Left err -> respondError err

respondError :: Error -> APIMonad url session Response
respondError = ok' . show 
