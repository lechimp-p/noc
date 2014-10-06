{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module API.Happstack where

import API.Effects
import API.Config
import API.Session

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
import Happstack.Server.Response (resp, ToMessage(..))
import Happstack.Server.Types (Response, takeRequestBody)
import Happstack.Server.Monads (FilterMonad, ServerMonad, WebMonad)

runAPI :: ( Typeable1 m 
          , Functor m
          , MonadIO m
          , FilterMonad Response m
          , HasRqData m
          , ServerMonad m
          , WebMonad Response m
          , MonadClientSession AuthData m
          , MonadPlus m
          , SetMember Lift (Lift m) r
          )
       => APIConfig -> Eff (API :> r) a -> Eff r ()
runAPI config action = go (admin action)
    where
    go (Val v) = return () 
    go (E request) = handleRelay request go
        $ \ req -> case evalAPI config req of
            (True, action)  -> lift action >>= go 
            (False, action) -> lift action >> return () 

    
evalAPI :: ( Typeable1 m 
           , Functor m
           , MonadIO m
           , FilterMonad Response m
           , HasRqData m
           , ServerMonad m
           , WebMonad Response m
           , MonadClientSession AuthData m
           , MonadPlus m
           , SetMember Lift (Lift m) r
           )
        => APIConfig -> API (VE r w) -> (Bool, m (VE r w))
evalAPI config req = case req of
    GetSession n        -> (True, fmap n CS.getSession)
    PutSession s n      -> (True, fmap n (CS.putSession s))
    ExpireSession n     -> (True, fmap n CS.expireSession) 
    Respond s t n       -> (True, fmap n $ (resp s t) >> return ())
    LookGet t n         -> (True, fmap n (look t)) 
    Timestamp n         -> (True, fmap n (liftIO getCurrentTime))
    Config f n          -> (True, fmap n . return . f $ config)
    GetBody n           -> (True, fmap n $ do
                                    decodeBody bpol
                                    body <- askRq >>= liftIO . takeRequestBody
                                    case body of
                                        Just b -> return . unBody $ b
                                        Nothing -> return ""
                           )
    Abort n             -> (False, fmap n $ return undef)
    WriteFile p c n     -> undefined "Happstack.WriteFile"
    RemoveFile p n      -> undefined "Happstack.RemoveFile"
    where
    undef = error "evalAPI.respondNow: Don't eval this!"
    bpolc = _bodyPolicy config
    bpol = defaultBodyPolicy (_uploadPath bpolc)
                             (_maxBytesFile bpolc)
                             (_maxBytesBody bpolc)
                             (_maxBytesHeader bpolc)


data Message = forall a. IsResponse a => Message a

instance ToMessage Message where
    toContentType (Message a) = contentType a
    toMessage (Message a) = content a


