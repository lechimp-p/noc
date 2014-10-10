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
import Data.Scientific (isInteger, toBoundedInteger)
import Control.Lens
import Data.Data (Typeable)
import Data.Time.Clock (UTCTime)
import Data.Monoid


ifIsJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
ifIsJust v op =
    case v of
        Just a -> op a
        Nothing -> return () 

ifIsJust' = flip ifIsJust

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


normalizeResponse :: Member API r
                  => Eff r (Either Error (Maybe Value)) -> Eff r (Maybe Value)
normalizeResponse m = m >>= \ val -> case val of
    Left err        -> badRequest . Just $ errorJSON err
    Right Nothing   -> noContent $ Nothing
    Right v@(Just _)-> ok $ v 

---------------
-- JSON Helpers
---------------

withJSONIO :: Member API r
           => Eff (JSONOut :> JSONIn :> r) a -> Eff r (Either Error (Maybe Value)) 
withJSONIO eff = do
    body <- getBody
    fmap (fmapl JSONError' . fmap (Just . fst)) $ runJSONIO' body eff


withJSONOut :: Eff (JSONOut :> r) a -> Eff r (Either Error (Maybe Value))
withJSONOut = fmap (Right . Just . fst) . runJSONOut

withJSONIn :: Member API r
           => Eff (JSONIn :> r) a -> Eff r (Either Error a)
withJSONIn eff = do
    body <- getBody
    fmap (fmapl JSONError') $ runJSONIn' body eff

-----------------
-- JSON instances
-----------------

{--instance FromJSON Login where
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
--}

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

-----------------
-- Common outputs
-----------------

userInfo uid = do
    "id"            <: uid
    "login"         <$ getUserLogin uid
    "name"          <$ getUserName uid
    "description"   <$ getUserDesc uid
    "icon"          <$ getUserIcon uid

channelInfo cid = do
    "id"            <: cid
    "name"          <$ getChanName cid
    "description"   <$ getChanDesc cid      
    "type"          <$ getChanType cid
    "amountOfUsers" <$ amountOfSubscribedUsers cid
    "lastPost"      <$ lastPostTimestamp cid
