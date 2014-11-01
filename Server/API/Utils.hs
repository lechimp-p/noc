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
import Model.Permissions (hasAccess, forUserSelfOrAdmins)
import qualified Model.Query as QueryUnsecure
import qualified Model.Errors as ME
import Model.BaseTypes
import API.Config
import API.Effects
import API.ImageUtils

import Control.Eff
import Control.Eff.JSON
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L
import Data.Aeson 
import Data.Aeson.Types
import qualified Data.Aeson.TH as TH 
import Control.Monad (mzero)
import Data.Scientific (isInteger, toBoundedInteger)
import Control.Lens hiding ((.=))
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
-- Common outputs
-----------------

userInfo uid = do
    "id"            <: uid
    "login"         <$ getUserLogin uid
    "name"          <$ getUserName uid
    "description"   <$ getUserDesc uid
    "icon"          <$ getUserIcon uid
    sp <- hasAccess uid forUserSelfOrAdmins
    if sp
        then "email" <$ getUserEmail uid >> return ()
        else return ()
    oid <- forceOperatorId
    "contact"       <$ fmap (fmap _channelId) .$ getUserContactByContactId oid uid

channelInfo cid = do
    uid <- forceOperatorId
    "id"            <: cid
    "name"          <$ do
        t <- getChanType cid
        case t of
            Conversation    -> do
                subsc <- fmap (S.delete uid) $ QueryUnsecure.getChanSubscribers cid
                names <- sequence . (fmap getUserName) .S.toList $ subsc
                let n = mconcat . L.intersperse ", " . fmap nameToText $ names
                makeName n
            otherwise       -> getChanName cid
    "description"   <$ getChanDesc cid      
    "type"          <$ getChanType cid
    "image"         <$ getChanImage cid
    "amountOfUsers" <$ amountOfSubscribedUsers cid
    "lastPost"      <$ lastPostTimestamp cid
    "subscribed"    <$ fmap (S.member cid) .$ getUserSubscriptions uid 

messageJSON msg = do
    "image"     <: _image msg
    "text"      <: _text msg
    "timestamp" <: show .$ _timestamp msg
    let uid = _author msg  
    "author"    <$. userInfo uid


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
errorJSON err = object $ ("error" .= errorType err) : moreInfo err

errorType :: Error -> String
errorType err = case err of
    (ModelError' err) -> "ModelError/" ++ modelErrorType err
    (JSONError' err)  -> "JSONError/" ++ jsonErrorType err
    (ImageError' err) -> "ImageError/" ++ imageErrorType err
    MethodNotSupported  -> "MethodNotSupported"
    JustStopped -> "JustStopped"

modelErrorType :: ME.Error -> String
modelErrorType err = case err of
    (ME.InsufficientPermissions _) -> "InsufficientPermissions" 
    (ME.UnknownChannel _) -> "UnknownChannel"
    (ME.UnknownUser _) -> "UnknownUser"
    (ME.UnknownLogin _) -> "UnknownLogin"
    (ME.UnknownMessage _) -> "UnknownMessage"
    (ME.OnlyOneChannelOwnerLeft _) -> "OnlyOneChannelOwnerLeft"
    (ME.OnlyOneNoCAdminLeft) -> "OnlyOneNoCAdminLeft"
    (ME.DuplicateLogin _) -> "DuplicateLogin"
    (ME.CantLogin _) -> "CantLogin"
    (ME.AlreadyLoggedIn) -> "AlreadyLoggedIn"
    (ME.NotLoggedIn) -> "NotLoggedIn"
    (ME.ConstraintViolation _) -> "ConstraintViolation"
    (ME.Custom _) -> "Custom"

jsonErrorType :: JSONError -> String
jsonErrorType err = case err of
    (MissingProperty _) -> "MissingProperty" 
    (CantDecodeProperty _ _) -> "CantDecodeProperty"
    (CantDecodeObject _) -> "CantDecodeObject" 

imageErrorType :: ImageError -> String
imageErrorType err = case err of
    (Base64Error _) -> "Base64Error"
    (DecodingError _) -> "DecodingError"
    (EncodingError _) -> "EncodingError"
    (ResizeError _) -> "ResizeError"

moreInfo :: Error -> [Pair]
moreInfo err = case err of
    (ModelError' err) -> modelErrorInfo err
    (JSONError' err)  -> jsonErrorInfo err
    (ImageError' err) -> imageErrorInfo err
    otherwise -> []

modelErrorInfo :: ME.Error -> [Pair]
modelErrorInfo err = case err of
    (ME.InsufficientPermissions p) -> ["violations" .= toJSON (permissionViolationInfo p)]
    (ME.UnknownChannel v) -> ["chanId" .= toJSON (ciToInt v)]
    (ME.UnknownUser v) -> ["userId" .= toJSON (uiToInt v)]
    (ME.UnknownLogin v) -> ["login" .= toJSON v]
    (ME.UnknownMessage v) -> ["msgId" .= toJSON (miToInt v)]
    (ME.OnlyOneChannelOwnerLeft v) -> ["chanId" .= toJSON (ciToInt v)]
    (ME.OnlyOneNoCAdminLeft) -> []
    (ME.DuplicateLogin v) -> ["login" .= toJSON (loginToText v)]
    (ME.CantLogin v) -> ["login" .= toJSON (loginToText v)]
    (ME.AlreadyLoggedIn) -> []
    (ME.NotLoggedIn) -> []
    (ME.ConstraintViolation v) -> ["reason" .= toJSON v] 
    (ME.Custom v) -> ["reason" .= toJSON v] 

permissionViolationInfo :: ME.PermissionViolation -> [Value] 
permissionViolationInfo viol = case viol of
    ME.JustForbidden
        -> [object [ "reason" .= toJSON ("just forbidden" :: Text)]]
    (ME.NoNoCAdmin v)
        -> [object [ "reason" .= toJSON ("no noc admin" :: Text)
                   , "userId" .= toJSON (uiToInt v) ]]
    (ME.NoChanAdmin v v')
        -> [object [ "reason" .= toJSON ("no channel admin" :: Text)
                   , "opId" .= toJSON (uiToInt v)
                   , "chanId" .= toJSON (ciToInt v') ]]
    (ME.NoChanOwner v v')
        ->  [object [ "reason" .= toJSON ("no channel owner" :: Text)
                    , "opId" .= toJSON (uiToInt v)
                    , "chanId" .= toJSON (ciToInt v') ]]
    (ME.NoChanProducer v v')
        -> [object [ "reason" .= toJSON ("no channel producer" :: Text)
                   , "opId" .= toJSON (uiToInt v)
                   , "chanId" .= toJSON (ciToInt v') ]]
    (ME.NoChanConsumer v v')
        -> [object [ "reason" .= toJSON ("no channel consumer" :: Text)
                   , "opId" .= toJSON (uiToInt v)
                   , "chanId" .= toJSON (ciToInt v') ]]
    (ME.NoUserAdmin v v')
        -> [object [ "reason" .= toJSON ("no user admin" :: Text)
                   , "opId" .= toJSON (uiToInt v)
                   , "userId" .= toJSON (uiToInt v') ]]
    (ME.NoUserSelf v v')
        -> [object [ "reason" .= toJSON ("not user herself" :: Text)
                   , "opId" .= toJSON (uiToInt v)
                   , "userId" .= toJSON (uiToInt v') ]]
    (ME.NotOnContactList v v')
        -> [object [ "reason" .= toJSON ("not on contact list" :: Text)
                   , "opId" .= toJSON (uiToInt v)
                   , "userId" .= toJSON (uiToInt v') ]]
    (ME.PVAnd v v')
        -> permissionViolationInfo v ++ permissionViolationInfo v'

jsonErrorInfo :: JSONError -> [Pair] 
jsonErrorInfo err = case err of
    (MissingProperty v) -> ["name" .= toJSON v] 
    (CantDecodeProperty v v') -> ["name" .= toJSON v, "reason" .= toJSON v']
    (CantDecodeObject _) -> [] 

imageErrorInfo :: ImageError -> [Pair] 
imageErrorInfo err = case err of
    (Base64Error v) -> ["reason" .= toJSON v]  
    (DecodingError v) -> ["reason" .= toJSON v] 
    (EncodingError v) -> ["reason" .= toJSON v]
    (ResizeError v) -> ["reason" .= toJSON v]

normalizeError :: Either Error (Maybe Value) -> (Maybe Value)
normalizeError (Left err) = Just $ errorJSON err
normalizeError (Right v) = v

normalizeResponse :: Member API r
                  => Eff r (Either Error (Maybe Value)) -> Eff r (Maybe Value)
normalizeResponse m = m >>= \ val -> case val of
    Left err@(ModelError' ME.NotLoggedIn) -> unauthorized . Just $ errorJSON err 
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

$(TH.deriveJSON TH.defaultOptions{TH.fieldLabelModifier = drop 1} ''Contact)
