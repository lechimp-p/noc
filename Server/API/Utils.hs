{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module API.Utils
where

import Model
import qualified Model.Errors as ME
import Model.BaseTypes
import API.Config
import API.Effects
import API.ImageUtils

import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString as BL 
import Control.Eff
import Control.Eff.JSON
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value (..), FromJSON, ToJSON
                  , parseJSON, toJSON)
import Control.Monad (mzero)
--import Data.Aeson.Types
import Data.Scientific (isInteger, toBoundedInteger)
import Control.Lens
--import Control.Lens.Prism
import Data.Data (Typeable)
import Data.Time.Clock (UTCTime)
import Data.Monoid


ifIsJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
ifIsJust v op =
    case v of
        Just a -> op a
        Nothing -> return () 

ifIsJust' = flip ifIsJust


withJSONIO :: Member API r
           => Eff (JSONOut :> JSONIn :> r) a -> Eff r (Either Error (Maybe Value)) 
withJSONIO eff = do
    body <- getBody
    fmap (fmapl JSONError' . fmap (Just . fst)) $ runJSONIO' body eff --(if L.null body then "{}" else body) eff


withJSONOut :: Eff (JSONOut :> r) a -> Eff r (Either Error (Maybe Value))
withJSONOut = fmap (Right . Just . fst) . runJSONOut


withJSONIn :: Member API r
           => Eff (JSONIn :> r) a -> Eff r (Either Error a)
withJSONIn eff = do
    body <- getBody
    fmap (fmapl JSONError') $ runJSONIn' body eff --(if L.null body then "{}" else body) eff


-----------------
-- Error handling
-----------------

data Error
    = ModelError' ME.Error
    | JSONError' JSONError
    | ImageError' ImageError
    | MethodNotSupported
    | JustStopped
    deriving (Show)

instance Monoid Error where
    mempty = JustStopped 
    a `mappend` _ = a

fmapl :: (a -> b) -> Either a c -> Either b c
fmapl f (Right v) = Right v
fmapl f (Left v) = Left . f $ v

methodNotSupported :: Member API r
                   => Eff r (Either Error (Maybe Value))
methodNotSupported = return . Left $ MethodNotSupported

errorJSON :: Error -> Value
errorJSON = String . T.pack . show

normalizeError :: Either Error (Maybe Value) -> (Maybe Value)
normalizeError (Left err) = Just $ errorJSON err
normalizeError (Right v) = v

-----------------
-- JSON instances
-----------------

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

instance FromJSON ChanType where
    parseJSON (String t)
        | any (t == ) ["none", "None", "NONE"] = return None
        | any (t == ) ["stream", "Stream", "STREAM"] = return Stream 
        | any (t == ) ["conversation", "Conversation", "CONVERSATION"] = return Conversation
        | otherwise = mzero
    parseJSON _ = mzero

instance FromJSON UserId where
    parseJSON (Number s)
        | not . isInteger $ s = mzero
        | otherwise =
            let res = toBoundedInteger s
            in case res of
                Nothing  -> mzero
                Just uid -> return . UserId $ uid
    parseJSON _ = mzero

instance FromJSON ChanId where
    parseJSON (Number s)
        | not . isInteger $ s = mzero
        | otherwise =
            let res = toBoundedInteger s
            in case res of
                Nothing  -> mzero
                Just cid -> return . ChanId $ cid
    parseJSON _ = mzero

instance ToJSON UserId where
    toJSON = toJSON . uiToInt 

instance ToJSON ChanId where
    toJSON = toJSON . ciToInt 

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

instance ToJSON ChanType where
    toJSON None         = String "none"
    toJSON Stream       = String "stream"
    toJSON Conversation = String "conversation"

--instance ToMessage Value where
--    toContentType _ = B.pack "application/json"
--    toMessage       = encode


{--instance (Monad m, MonadIO m)
     => FilterMonad Response (JSONQueryMonadT acid url session m) where
    setFilter = JSONQueryMonadT . lift . lift . lift . setFilter
    composeFilter = JSONQueryMonadT . lift . lift . lift . composeFilter
    -- getFilter :: m b -> m (b, a -> a)
    getFilter m = do
        obj <- JSONQueryMonadT . JSONMonadT $ ask
        uid <- JSONQueryMonadT . lift $ maybeOperatorIdQ
        acid <- JSONQueryMonadT . lift $ getAcidQ
        (((a', obj'), uid'), res) <- JSONQueryMonadT . lift . lift 
                . getFilter . queryWithJSON' acid uid obj $ m
        case obj' of
            Object o -> JSONQueryMonadT . JSONMonadT . lift . tell $ HM.toList o
            otherwise -> error "API.Utils.getFilter (QueryMonadT): this should not happen."
        JSONQueryMonadT . lift $ setOperatorIdQ uid'
        return (a', res)

instance ( Monad m, MonadIO m, Functor m
         , ClientSession session )
      => MonadClientSession session (JSONQueryMonadT acid url session m)
    where 
    getSession = JSONQueryMonadT . lift . lift . lift $ getSession
    putSession = JSONQueryMonadT . lift . lift . lift . putSession
    expireSession = JSONQueryMonadT . lift . lift . lift $ expireSession
--}

{--queryWithJSON' :: (Monad m, MonadIO m)
               => AcidState acid
               -> Maybe UserId
               -> Object
               -> JSONQueryMonadT acid url session m a
               -> EitherT Error (APIMonadT url session m) ((a, Value), Maybe UserId)
queryWithJSON' acid uid obj json = do
    (\ m -> runQueryMonadT' m acid uid) 
        . (\ m -> runJSONMonadTWithObject m obj) 
        . runJSONQueryMonadT 
        $ json 

queryWithJSON :: (Monad m, MonadIO m)
              => AcidState acid
              -> JSONQueryMonadT acid url session m a
              -> EitherT Error (APIMonadT url session m) (a, Value)
queryWithJSON acid json = do
    body <- getBody
    flip runQueryMonadT acid 
        . flip runJSONMonadT (if BL.null body then "{}" else body) 
        . runJSONQueryMonadT 
        $ json 

queryWithJSONInput acid json = queryWithJSON acid json >>= return . fst 
queryWithJSONResponse acid json = do
    (_, res) <- queryWithJSON acid json
    jsonR' res
--}

{--
instance (Monad m, MonadIO m)
     => FilterMonad Response (JSONUpdateMonadT acid url session m) where
    setFilter = JSONUpdateMonadT . lift . lift . lift . setFilter
    composeFilter = JSONUpdateMonadT . lift . lift . lift . composeFilter
    -- getFilter :: m b -> m (b, a -> a)
    getFilter m = do
        obj <- JSONUpdateMonadT . JSONMonadT $ ask
        uid <- JSONUpdateMonadT . lift $ maybeOperatorIdU
        acid <- JSONUpdateMonadT . lift $ getAcidU
        (((a', obj'), uid'), res) <- JSONUpdateMonadT . lift . lift 
                . getFilter . updateWithJSON' acid uid obj $ m
        case obj' of
            Object o -> JSONUpdateMonadT . JSONMonadT . lift . tell $ HM.toList o
            otherwise -> error "API.Utils.getFilter (UpdateMonadT): this should not happen."
        JSONUpdateMonadT . lift $ setOperatorIdU uid'
        return (a', res)

instance ( Monad m, MonadIO m, Functor m
         , ClientSession session)
      => MonadClientSession session (JSONUpdateMonadT acid url session m)
    where 
    getSession = JSONUpdateMonadT . lift . lift . lift $ getSession
    putSession = JSONUpdateMonadT . lift . lift . lift . putSession
    expireSession = JSONUpdateMonadT . lift . lift . lift $ expireSession

instance (Monad m, MonadIO m)
      => MonadError Error (JSONUpdateMonadT acid url session m) where
    throwError = JSONUpdateMonadT . lift . lift . throwError
    catchError op handler = do
        obj <- JSONUpdateMonadT . JSONMonadT $ ask
        uid <- JSONUpdateMonadT . lift $ maybeOperatorIdU
        acid <- JSONUpdateMonadT . lift $ getAcidU
        ((a', obj'), uid') <- JSONUpdateMonadT . lift . lift
                . catchError (updateWithJSON' acid uid obj op)
                           $ (\ e -> updateWithJSON' acid uid obj (handler e))
        case obj' of
            Object o -> JSONUpdateMonadT . JSONMonadT . lift . tell $ HM.toList o
            otherwise -> error "API.Utils.catchError (UpdateMonadT): this should not happen."
        JSONUpdateMonadT . lift $ setOperatorIdU uid'
        return a'
--}
{--
updateWithJSON' :: (Monad m, MonadIO m)
               => AcidState acid
               -> Maybe UserId
               -> Object
               -> JSONUpdateMonadT acid url session m a
               -> EitherT Error (APIMonadT url session m) ((a, Value), Maybe UserId)
updateWithJSON' acid uid obj json = do
    (\ m -> runUpdateMonadT' m acid uid) 
        . (\ m -> runJSONMonadTWithObject m obj) 
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

updateWithJSONInput :: (Monad m, MonadIO m)
                    => AcidState acid
                    -> JSONUpdateMonadT acid url session m a
                    -> EitherT Error (APIMonadT url session m) a
updateWithJSONInput acid json = updateWithJSON acid json >>= return . fst 
updateWithJSONResponse acid json = do
    (_, res) <- updateWithJSON acid json
    jsonR' res
--}

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

userInfo uid = do
    "id"        <: uid
    "login"     <$ getUserLogin uid
    "name"      <$ getUserName uid
    "icon"      <$ getUserIcon uid

channelInfo cid = do
    "id"            <: cid
    "name"          <$ getChanName cid
    "description"   <$ getChanDesc cid      
    "type"          <$ getChanType cid
