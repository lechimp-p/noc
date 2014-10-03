{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Happstack where

import API.Effects

import Control.Eff
import Control.Eff.Lift
import Data.Data (Typeable)
import Data.Time.Clock (getCurrentTime)
import Data.Typeable.Internal (Typeable1)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (MonadPlus)
import Happstack.Server.ClientSession as CS
import Happstack.Server (unBody, askRq)
import Happstack.Server.RqData ( look
                               , decodeBody
                               , defaultBodyPolicy
                               , BodyPolicy
                               , HasRqData
                               )
import Happstack.Server.Response (resp)
import Happstack.Server.Types (Response, takeRequestBody)
import Happstack.Server.Monads (FilterMonad, ServerMonad, WebMonad)

runAPI :: ( Typeable session
          , Typeable config 
          , Typeable1 m 
          , Functor m
          , MonadIO m
          , CS.MonadClientSession session m
          , FilterMonad Response m
          , HasRqData m
          , ServerMonad m
          , WebMonad Response m
          , MonadPlus m
          , SetMember Lift (Lift m) r
          )
       => config -> (config -> BodyPolicy)
       -> Eff ((API session config) :> r) a -> Eff r ()
runAPI config bpl action = go (admin action)
    where
    go (Val v) = return () 
    go (E request) = handleRelay request go
        $ \ req -> case evalAPI config bpl req of
            (True, action)  -> lift action >>= go 
            (False, action) -> lift action >> return () 
    
evalAPI :: ( Typeable session
           , Typeable config 
           , Typeable1 m 
           , Functor m
           , MonadIO m
           , CS.MonadClientSession session m
           , FilterMonad Response m
           , HasRqData m
           , ServerMonad m
           , WebMonad Response m
           , MonadPlus m
           , SetMember Lift (Lift m) r
           )
        => config -> (config -> BodyPolicy)
        -> API session config (VE r w) -> (Bool, m (VE r w))
evalAPI config bpl req = case req of
    GetSession n        -> (True, fmap n CS.getSession)
    PutSession s n      -> (True, fmap n (CS.putSession s))
    ExpireSession n     -> (True, fmap n CS.expireSession) 
    RespondNow s t n    -> (False, fmap n $ (resp s t) >> return undef)
    LookGet t n         -> (True, fmap n (look t)) 
    Timestamp n         -> (True, fmap n (liftIO getCurrentTime))
    Config n            -> (True, fmap n (return config))
    GetBody n           -> (True, fmap n $ do
                                    decodeBody . bpl $ config  
                                    body <- askRq >>= liftIO . takeRequestBody
                                    case body of
                                        Just b -> return . unBody $ b
                                        Nothing -> return ""
                           )
    where
    undef = error "evalAPI.respondNow: Don't eval this!"
{--    bpolc = _bodyPolicy config
    bpol = defaultBodyPolicy (_uploadPath bpolc)
                             (_maxBytesFile bpolc)
                             (_maxBytesBody bpolc)
                             (_maxBytesHeader bpolc)
--}


