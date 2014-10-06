{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.ImageUtils
    ( ImageError (..)
    , storeIcon
    , removeIcon
    , storeImage
    , removeImage
    , defaultConfig
    )
where

import Model
import Model.BaseTypes
import API.Config
import API.Effects

import Prelude hiding (writeFile)
import Data.Aeson hiding (decode)
import Control.Monad.IO.Class
import Control.Monad
import Data.Text hiding (any)
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Hashable
import Data.ByteString hiding (any, writeFile)
import Data.ByteString.Base64
import System.FilePath.Posix

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

defaultConfig = ImageConfig "./files" "user" "channel" 0

storeIcon uid typ dat = do
    cnfg <- config _imageConfig
    let sub = _userDir cnfg </> (show . uiToInt $ uid)
    path <- storeGeneric cnfg sub typ dat 
    return . fmap (Icon . T.pack) $ path
removeIcon = removeFile . T.unpack . icnPath

storeImage cnfg typ dat = do
    cnfg <- config _imageConfig
    path <- storeGeneric cnfg (_channelDir cnfg) typ dat 
    return . fmap (Image . T.pack) $ path
removeImage = removeFile . T.unpack . imgPath 

storeGeneric :: Member API r 
             => ImageConfig -> FilePath -> ImgType -> Text -> Eff r (Either ImageError String)
storeGeneric cnfg path typ dat = do
--    base <- liftIO $ getCurrentDirectory
    let hash = hashWithSalt (_salt cnfg) dat
        filename = show hash <.> getExtension typ 
    imgDat <- decodeBase64 dat
    case imgDat of
        Right dat -> do
            -- further processing like scaling goes here
            writeFile (path </> filename) dat 
            return . Right $ path </> filename
        Left err -> do
            return $ Left err

--removeGeneric :: Member API r 
--           => FilePath -> Eff r Bool 
--removeGeneric = removeFile 

decodeBase64 :: Member API r 
             => Text -> Eff r (Either ImageError ByteString)
decodeBase64 dat = do
    let dcd = decode . encodeUtf8 $ dat  
    case dcd of
        Left err -> return . Left . Base64Error $ err
        Right res -> return $ Right res 
