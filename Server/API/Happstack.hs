{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Happstack where

import API.Effects
import API.Config
import API.Session
import Model.Acid.SafeCopy

import Control.Eff
import Control.Eff.Lift
import Data.Data (Typeable)
import Data.Typeable (Typeable1)
import Data.Time.Clock (getCurrentTime)
import Data.Typeable.Internal (Typeable1)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.ByteString (writeFile)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (MonadPlus)
import Control.Lens
import System.FilePath.Posix
import System.Directory ( getCurrentDirectory
                        , createDirectoryIfMissing
                        , removeFile
                        )
import Happstack.Server.ClientSession as CS
import Happstack.Server (unBody, askRq, ServerPartT)
import Happstack.Server.RqData ( looks
                               , decodeBody
                               , defaultBodyPolicy
                               , BodyPolicy
                               , HasRqData
                               , queryString
                               )
import Happstack.Server.Response (resp, ToMessage(..))
import Happstack.Server.Types (Response, takeRequestBody)
import Happstack.Server.Monads (FilterMonad, ServerMonad, WebMonad)
import qualified Happstack.Server.Types as HS

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
       => APIConfig -> Eff (API :> r) a -> Eff r a
runAPI config action = go (admin action)
    where
    go (Val v) = return v 
    go (E request) = handleRelay request go
        $ \ req -> (lift (evalAPI config req) >>= go)

    
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
        => APIConfig -> API (VE r w) -> (m (VE r w))
evalAPI config req = case req of
    GetSession n        -> (fmap n CS.getSession)
    PutSession s n      -> (fmap n (CS.putSession s))
    ExpireSession n     -> (fmap n CS.expireSession) 
    Respond s t n       -> (fmap n $ (resp s t))
    LookGet t n         -> (fmap (n . head') (queryString $ looks t)) 
    Timestamp n         -> (fmap n (liftIO getCurrentTime))
    Config f n          -> (fmap n . return . f $ config)
    GetBody n           -> (fmap n $ do
                                    decodeBody bpol
                                    body <- askRq >>= liftIO . takeRequestBody
                                    case body of
                                        Just b -> return . unBody $ b
                                        Nothing -> return ""
                           )
    Method n            -> (fmap n $ do
                                rq <- askRq
                                return $ case HS.rqMethod rq of
                                    HS.GET -> GET
                                    HS.HEAD -> HEAD
                                    HS.POST -> POST
                                    HS.PUT -> PUT
                                    HS.DELETE -> DELETE
                                    HS.TRACE -> TRACE
                                    HS.OPTIONS -> OPTIONS
                                    HS.CONNECT -> CONNECT
                           )
--    Abort n             -> (False, fmap n $ return undef)
    WriteFile p c n     -> (fmap n $ do
                                base <- basepath
                                let dir = base </> takeDirectory p
                                liftIO $ createDirectoryIfMissing True dir                                
                                liftIO $ Data.ByteString.writeFile (base </> p) c
                                return True 
                           )
    RemoveFile p n      -> (fmap n $ do
                                base <- basepath
                                liftIO . System.Directory.removeFile $ base </> p
                                return True 
                           )
    where
    undef = error "evalAPI.respondNow: Don't eval this!"
    bpolc = _bodyPolicy config
    bpol = defaultBodyPolicy (_uploadPath bpolc)
                             (_maxBytesFile bpolc)
                             (_maxBytesBody bpolc)
                             (_maxBytesHeader bpolc)
    basepath' = config ^. imageConfig . basePath      
    dontExpandBasePath = not $ head basepath' == '.' 
    basepath = 
        if dontExpandBasePath
        then return basepath'
        else do
            b <- liftIO getCurrentDirectory
            return $ b </> basepath'

    head' [] = Nothing
    head' (x:xs) = Just x
    

data Message = forall a. IsResponse a => Message a

instance ToMessage Message where
    toContentType (Message a) = contentType a
    toMessage (Message a) = content a

makeResponse :: IsResponse a => a -> Response
makeResponse = toResponse . Message

$(deriveSafeCopy 0 'base ''AuthData)

instance CS.ClientSession AuthData where
    emptySession = AuthData Nothing Nothing Nothing

-- This is necessary since GHC won't derive a Typeable1 instance for plain 
-- transformder stack
newtype CSSPT a = CSSPT { unCSSPT :: CS.ClientSessionT AuthData (ServerPartT IO) a}
    deriving (Typeable, Monad, Functor, MonadIO
             , MonadClientSession AuthData
             , FilterMonad Response
             , HasRqData
             , ServerMonad
             , WebMonad Response
             , MonadPlus
             )
