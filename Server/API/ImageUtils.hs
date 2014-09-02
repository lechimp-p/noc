{-# LANGUAGE OverloadedStrings #-}

module API.ImageUtils
    ( ImageError (..)
    , MonadImageError
    , throwImageError
    , storeIcon
    , removeIcon
    , storeImage
    , removeImage
    , defaultConfig
    )
where

import Prelude hiding (writeFile)
import Data.Aeson hiding (decode)
import Control.Monad.IO.Class
import Control.Monad
import Data.Text hiding (any)
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Hashable
import Data.ByteString hiding (any)
import Data.ByteString.Base64
import System.FilePath.Posix
import System.Directory

import Model
import Model.BaseTypes
import API.Config (ImageConfig(..))

data ImgType 
    = PNG
    | JPEG
    deriving (Show)

instance FromJSON ImgType where
    parseJSON (String t) 
        | any (t == ) ["png", "PNG"] = return PNG
        | any (t == ) ["jpeg", "jpg", "JPEG", "JPG"] = return JPEG
        | otherwise = mzero
    parseJSON _  = mzero

getExtension typ = case typ of
    PNG     -> "png"
    JPEG    -> "jpeg"

data ImageError 
    = Base64Error String
    deriving (Show)

class MonadImageError m where
    throwImageError :: ImageError -> m a 

defaultConfig = ImageConfig "./files" "user" "channel" 0

storeIcon cnfg uid typ dat = do
    let sub = _userDir cnfg </> (show . uiToInt $ uid)
    path <- storeGeneric cnfg sub typ dat 
    return . Icon . T.pack $ path
removeIcon cnfg = removeGeneric cnfg . T.unpack . icnPath

storeImage cnfg typ dat = do
    path <- storeGeneric cnfg (_channelDir cnfg) typ dat 
    return . Image . T.pack $ path
removeImage cnfg = removeGeneric cnfg . T.unpack . imgPath 

storeGeneric :: (MonadIO m, MonadImageError m) 
          => ImageConfig -> FilePath -> ImgType -> Text -> m String 
storeGeneric cnfg path typ dat = do
    base <- liftIO $ getCurrentDirectory
    let hash = hashWithSalt (_salt cnfg) dat
        path' = _basePath cnfg </> path 
        filename = show hash <.> getExtension typ 
    imgDat <- decodeBase64 dat
    liftIO $ createDirectoryIfMissing True (base </> path')
    -- further processing like scaling goes here
    liftIO $ writeFile (base </> path' </> filename) imgDat
    return (path </> filename) 

removeGeneric :: (MonadIO m, MonadImageError m) 
           => ImageConfig -> String -> m ()
removeGeneric cnfg path = liftIO . removeFile $ (_basePath cnfg) </> path 

decodeBase64 :: (MonadIO m, MonadImageError m) 
             => Text -> m ByteString
decodeBase64 dat = do
    let dcd = decode . encodeUtf8 $ dat  
    case dcd of
        Left err -> throwImageError . Base64Error $ err
        Right res -> return res 
