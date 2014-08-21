{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.Utils
where

import Data.Text
import Data.Acid ( AcidState )
import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString.Char8 as B 
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
import Control.Monad.Trans.Either
import Control.Monad.Error.Class hiding (Error)

import Model
import Model.BaseTypes
import API.Errors
import API.APIMonad
import API.JSONQueryMonad
import API.JSONUpdateMonad
import API.JSONUtils
import ACID

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

instance ToJSON UserId where
    toJSON = toJSON . uiToInt 

instance ToJSON MsgId where
    toJSON = toJSON . miToInt 

instance ToJSON Login where
    toJSON = String . loginToText

instance ToJSON Password where
    toJSON = String . pwToText

instance ToJSON Name where
    toJSON = String . nameToText

instance ToJSON Desc where
    toJSON = String . descToText

instance ToJSON Icon where
    toJSON = String . icnPath 

instance ToJSON Image where
    toJSON = String . imgPath 

instance ToMessage Value where
    toContentType _ = B.pack "application/json"
    toMessage       = encode

bodyPolicy = defaultBodyPolicy "/tmp/NoC-Server-dev"
                               100000 -- file upload
                               100000 -- no files
                               100000 -- overhead for multipart/form-data headers

getBody :: ( ServerMonad m, MonadPlus m, MonadIO m
           , FilterMonad Response m, WebMonad Response m) 
        => m L.ByteString
getBody = do
    decodeBody bodyPolicy
    body <- askRq >>= liftIO . takeRequestBody
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing -> return ""
     
ok' :: (Monad m, MonadIO m, FilterMonad Response m)
    =>  Text -> m Response
ok' = ok . toResponse 

noContent' :: (Monad m, MonadIO m, FilterMonad Response m)
           => m Response
noContent' = noContent . toResponse $ ("" :: Text)

badRequest' :: (Monad m, MonadIO m, FilterMonad Response m)
            =>  Text -> m Response
badRequest' = badRequest . toResponse 

jsonR' :: FilterMonad Response m => Value -> m Response
jsonR' = ok . toResponse 

ifIsJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
ifIsJust v op =
    case v of
        Just a -> op a
        Nothing -> return () 

ifIsJust' = flip ifIsJust

instance (Monad m, MonadIO m)
     => FilterMonad Response (JSONQueryMonadT acid url session m) where
    setFilter = JSONQueryMonadT . lift . lift . lift . setFilter
    composeFilter = JSONQueryMonadT . lift . lift . lift . composeFilter
    -- getFilter :: m b -> m (b, a -> a)
    getFilter m = do
        obj <- JSONQueryMonadT $ getObject
        ps <- JSONQueryMonadT $ getPairs   
        uid <- JSONQueryMonadT . lift $ maybeOperatorIdQ
        acid <- JSONQueryMonadT . lift $ getAcidQ
        (((a', ps'), uid'), res) <- JSONQueryMonadT . lift . lift 
                . getFilter . queryWithJSON' acid uid obj ps $ m
        JSONQueryMonadT $ setPairs ps'
        JSONQueryMonadT . lift $ setOperatorIdQ uid'
        return (a', res)

instance ( Monad m, MonadIO m, Functor m
         , ClientSession session )
      => MonadClientSession session (JSONQueryMonadT acid url session m)
    where 
    getSession = JSONQueryMonadT . lift . lift . lift $ getSession
    putSession = JSONQueryMonadT . lift . lift . lift . putSession
    expireSession = JSONQueryMonadT . lift . lift . lift $ expireSession

queryWithJSON' :: (Monad m, MonadIO m)
               => AcidState acid
               -> Maybe UserId
               -> Object
               -> [Pair]
               -> JSONQueryMonadT acid url session m a
               -> EitherT Error (APIMonadT url session m) ((a, [Pair]), Maybe UserId)
queryWithJSON' acid uid obj ps json = do
    body <- getBody
    (\ m -> runQueryMonadT' m acid uid) 
        . (\ m -> runJSONMonadT' m obj ps) 
        . runJSONQueryMonadT 
        $ json 

queryWithJSON :: (Monad m, MonadIO m)
              => AcidState acid
              -> JSONQueryMonadT acid url session m a
              -> EitherT Error (APIMonadT url session m) (a, Value)
queryWithJSON acid json = do
    body <- getBody
    flip runQueryMonadT acid 
        . flip runJSONMonadT body 
        . runJSONQueryMonadT 
        $ json 

queryWithJSONInput acid json = queryWithJSON acid json >>= return . fst 
queryWithJSONResponse acid json = do
    (_, res) <- queryWithJSON acid json
    jsonR' res



instance (Monad m, MonadIO m)
     => FilterMonad Response (JSONUpdateMonadT acid url session m) where
    setFilter = JSONUpdateMonadT . lift . lift . lift . setFilter
    composeFilter = JSONUpdateMonadT . lift . lift . lift . composeFilter
    -- getFilter :: m b -> m (b, a -> a)
    getFilter m = do
        obj <- JSONUpdateMonadT $ getObject
        ps <- JSONUpdateMonadT $ getPairs   
        uid <- JSONUpdateMonadT . lift $ maybeOperatorIdU
        acid <- JSONUpdateMonadT . lift $ getAcidU
        (((a', ps'), uid'), res) <- JSONUpdateMonadT . lift . lift 
                . getFilter . updateWithJSON' acid uid obj ps $ m
        JSONUpdateMonadT $ setPairs ps'
        JSONUpdateMonadT . lift $ setOperatorIdU uid'
        return (a', res)

instance ( Monad m, MonadIO m, Functor m
         , ClientSession session )
      => MonadClientSession session (JSONUpdateMonadT acid url session m)
    where 
    getSession = JSONUpdateMonadT . lift . lift . lift $ getSession
    putSession = JSONUpdateMonadT . lift . lift . lift . putSession
    expireSession = JSONUpdateMonadT . lift . lift . lift $ expireSession

instance (Monad m, MonadIO m)
      => MonadError Error (JSONUpdateMonadT acid url session m) where
    throwError = JSONUpdateMonadT . lift . lift . throwError
    catchError op handler = do
        obj <- JSONUpdateMonadT $ getObject
        ps <- JSONUpdateMonadT $ getPairs
        uid <- JSONUpdateMonadT . lift $ maybeOperatorIdU
        acid <- JSONUpdateMonadT . lift $ getAcidU
        ((a', ps'), uid') <- JSONUpdateMonadT . lift . lift
                . catchError (updateWithJSON' acid uid obj ps op)
                           $ (\ e -> updateWithJSON' acid uid obj ps (handler e))
        JSONUpdateMonadT $ setPairs ps'
        JSONUpdateMonadT . lift $ setOperatorIdU uid'
        return a'

updateWithJSON' :: (Monad m, MonadIO m)
               => AcidState acid
               -> Maybe UserId
               -> Object
               -> [Pair]
               -> JSONUpdateMonadT acid url session m a
               -> EitherT Error (APIMonadT url session m) ((a, [Pair]), Maybe UserId)
updateWithJSON' acid uid obj ps json = do
    body <- getBody
    (\ m -> runUpdateMonadT' m acid uid) 
        . (\ m -> runJSONMonadT' m obj ps) 
        . runJSONUpdateMonadT 
        $ json 

updateWithJSON :: (Monad m, MonadIO m)
              => AcidState acid
              -> JSONUpdateMonadT acid url session m a
              -> EitherT Error (APIMonadT url session m) (a, Value)
updateWithJSON acid json = do
    body <- getBody
    flip runUpdateMonadT acid 
        . flip runJSONMonadT body 
        . runJSONUpdateMonadT 
        $ json 

updateWithJSONInput acid json = updateWithJSON acid json >>= return . fst 
updateWithJSONResponse acid json = do
    (_, res) <- updateWithJSON acid json
    jsonR' res


{--
updateWithJSON :: (Monad m, MonadIO m)
               => AcidState acid
               -> JSONMonadT (UpdateMonadT acid (APIMonadT url session m)) a
               -> APIMonadT url session m (Either Error (a, Value))
updateWithJSON acid json = do
    body <- getBody
    res' <- flip runUpdateMonadT acid . runJSONMonadT json $ body 
    case res' of
        Left err -> return . Left . ModelError' $ err
        Right (Left err) -> return . Left . JSONError' $ err
        Right (Right a) -> return . Right $ a

updateWithJSONInput acid json = updateWithJSON acid json >>= return . over _Right fst
updateWithJSONResponse acid json = do
    res <- updateWithJSON acid json
    case res of
        Left err -> return $ Left err
        Right (_, v) -> jsonR' v >>= return . Right
--}
