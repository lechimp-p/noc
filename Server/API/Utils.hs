{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Utils
where

import Data.Acid ( AcidState )
import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString.Char8 as B 
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Control.Lens
import Control.Lens.Prism
import Happstack.Server
import Happstack.Server.Types
import Happstack.Server.ClientSession
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class

import Model
import Model.BaseTypes
import API.Errors
import API.APIMonad
import API.JSONUtils
import ACID.QueryMonad

type ACID = AcidState NoC

instance FromJSON Login where
    parseJSON (String t) = return . mkLogin $ t
    parseJSON _ = mzero 

instance FromJSON Password where
    parseJSON (String t) = return . mkPassword $ t
    parseJSON _ = mzero


instance FromJSON Name where
    parseJSON (String t) = return . mkName $ t
    parseJSON _ = mzero 

instance FromJSON Desc where
    parseJSON (String t) = return . mkDesc $ t
    parseJSON _ = mzero 

instance ToJSON Login where
    toJSON = String . loginToText

instance ToJSON Password where
    toJSON = String . pwToText

instance ToJSON Name where
    toJSON = String . nameToText

instance ToJSON Desc where
    toJSON = String . descToText

bodyPolicy = defaultBodyPolicy "/tmp/NoC-Server-dev"
                               1000 -- file upload
                               1000 -- no files
                               1000 -- overhead for multipart/form-data headers

getBody :: (Monad m, MonadIO m) 
        => APIMonadT url session m L.ByteString
getBody = do
    body <- APIMonadT $ do
        decodeBody bodyPolicy
        askRq >>= liftIO . takeRequestBody
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing -> return ""
     
ok' :: (Monad m, MonadIO m)
    =>  String -> APIMonadT url session m Response
ok' = ok . toResponse . T.pack

noContent' :: (Monad m, MonadIO m)
           => APIMonadT url session m Response
noContent' = noContent . toResponse . T.pack $ ""

badRequest' :: (Monad m, MonadIO m)
            =>  String -> APIMonadT url session m Response
badRequest' = badRequest . toResponse . T.pack


type APIQueryMonadT url session m a = QueryMonadT NoC (APIMonadT url session m) a

okQ :: (Monad m, MonadIO m)
    => String -> APIQueryMonadT url session m Response
okQ = lift . ok'

noContentQ :: (Monad m, MonadIO m) 
           => APIQueryMonadT url session m Response
noContentQ = lift noContent'

badRequestQ :: (Monad m, MonadIO m) 
            => String -> APIQueryMonadT url session m Response
badRequestQ = lift . badRequest'

instance ToMessage Value where
    toContentType _ = B.pack "application/json"
    toMessage       = encode

jsonR' :: FilterMonad Response m => Value -> m Response
jsonR' = ok . toResponse 

ifIsJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
ifIsJust v op =
    case v of
        Just a -> op a
        Nothing -> return () 

queryWithJSON :: (Monad m, MonadIO m)
              => AcidState acid
              -> JSONMonadT (QueryMonadT acid (APIMonadT url session m)) a
              -> APIMonadT url session m (Either Error (a, Value))
queryWithJSON acid json = do
    body <- getBody
    res' <- flip runQueryMonadT acid . runJSONMonadT json $ body 
    case res' of
        Left err -> return . Left . ModelError $ err
        Right (Left err) -> return . Left . JSONError $ err
        Right (Right a) -> return . Right $ a

queryWithJSONInput acid json = queryWithJSON acid json >>= return . over _Right fst
queryWithJSONResponse acid json = do
    res <- queryWithJSON acid json
    case res of
        Left err -> return $ Left err
        Right (_, v) -> jsonR' v >>= return . Right
