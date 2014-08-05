{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module API.ACIDEvents
where

import Data.Acid 
        ( AcidState, makeAcidic
        , Query, Update
        , QueryEvent, UpdateEvent
        , EventState, EventResult
        )
import Data.Acid.Advanced ( query', update' )
import Control.Monad.Reader (ask)
import Control.Monad.State (put, get)
import Control.Monad.IO.Class

import Model
import Model.Errors

type ACID = AcidState NoC

--------------
-- Scaffolding
--------------

data Transaction a state where
    QueryTA     :: (QueryEvent event) => event -> Transaction (EventResult event) (EventState event)
    UpdateTA    :: (UpdateEvent event) => event -> Transaction (EventResult event) (EventState event)

getResult :: (MonadIO m) 
          => AcidState state 
          -> Transaction a state -> m a
getResult acid (QueryTA q) = query' acid q 
getResult acid (UpdateTA u) = update' acid u

-------
-- Auth
-------

loginTA :: Login -> Password -> Query NoC (Either Error Bool) 
loginTA = mkQuery (return True) 

----------
-- Helpers
----------

mkQuery :: Operation b -> Login -> Password 
        -> Query NoC (Either Error b) 
mkQuery op l pw = do
    noc <- ask
    case runOp noc l pw op of
        Left err    -> return $ Left err
        Right (_,v) -> return $ Right v


mkUpdate :: Operation b -> Login -> Password
         -> Update NoC (Either Error b)
mkUpdate op l pw = do
    noc <- get  
    case runOp noc l pw op of
        Left err        -> return $ Left err
        Right (noc',v)  -> put noc' >> return (Right v)
             

--------------------------
-- Turn it to Acidic Stuff
--------------------------

$(makeAcidic ''NoC ['loginTA])
