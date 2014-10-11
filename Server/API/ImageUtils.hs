{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.ImageUtils where

import Model
import Model.BaseTypes
import API.ImageConfig
import API.Effects
import API.Config

import Prelude hiding (writeFile)
import Data.Aeson hiding (decode)
import Control.Monad.IO.Class
import Control.Monad
import Data.Text hiding (any)
import Data.Text.Encoding
import qualified Data.Text as T
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

storeUserIcon uid t = fmap (fmap (Icon . T.pack))
                    . storeGeneric _userIconDir _userIconSizes (show uid) t
removeUserIcon = removeFile . T.unpack . icnPath

storeMsgImage n t = fmap (fmap (Image . T.pack))
                  . storeGeneric _msgImageDir _msgImageSizes n t
removeMsgImage = removeFile . T.unpack . imgPath 

storeChannelImage cid t = fmap (fmap (Image . T.pack))
                     . storeGeneric _channelImageDir _channelImageSizes (show cid) t 
removeChannelImage = removeFile . T.unpack . imgPath

storeGeneric :: Member API r 
             => (ImageConfig -> FilePath)
             -> (ImageConfig -> [ImageSize])
             -> String
             -> ImgType
             -> Text
             -> Eff r (Either ImageError String)
storeGeneric _path _sizes name typ dat = do
    cnfg <- config _imageConfig
    let path = _path cnfg
        sizes = _sizes cnfg
    imgDat <- decodeBase64 dat
    flip (either (return . Left)) imgDat $ \ imgDat' -> do
        let resized = fmap (resizeImage imgDat') sizes
            rs = Prelude.zip resized sizes
            p = path </> name
        sequence . flip fmap rs $ \(dat, size) ->
            writeFile (p <.> (getExtension typ ++ maybe "" id (_postfix size))) dat
        return . Right $ p <.> getExtension typ 

decodeBase64 :: Member API r 
             => Text -> Eff r (Either ImageError ByteString)
decodeBase64 dat = do
    let dcd = decode . encodeUtf8 $ dat  
    case dcd of
        Left err -> return . Left . Base64Error $ err
        Right res -> return $ Right res 

resizeImage :: ByteString -> ImageSize -> ByteString
resizeImage dat s = dat 
