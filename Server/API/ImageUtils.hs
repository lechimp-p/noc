{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API.ImageUtils where

import Debug.Trace

import Model
import Model.BaseTypes
import API.ImageConfig
import API.Effects
import API.Config

import System.IO.Error (tryIOError)
import System.IO.Unsafe (unsafePerformIO)
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
import qualified Graphics.GD as GD 

type ProcImage = GD.Image

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
    | DecodingError String
    | EncodingError String
    | ResizeError String
    deriving (Show)

storeUserIcon uid t = fmap (fmap (Icon . T.pack))
                    . storeGeneric _userIconDir _userIconSizes (show . uiToInt $ uid) t
removeUserIcon = removeFile . T.unpack . icnPath

storeMsgImage n t = fmap (fmap (Image . T.pack))
                  . storeGeneric _msgImageDir _msgImageSizes n t
removeMsgImage = removeFile . T.unpack . imgPath 

storeChannelImage cid t = fmap (fmap (Image . T.pack))
                     . storeGeneric _channelImageDir _channelImageSizes (show . ciToInt $ cid) t 
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
    check imgDat $ \ imgDat' -> do
        check (loadImage typ imgDat') $ \ img' -> do
            let resized = rights $ fmap (\s -> (resizeImage img' s, s)) sizes
                save = rights $ fmap (\(i,s) -> (saveImage typ i, s)) resized
                p = path </> name
            sequence . flip fmap save $ \(dat, size) ->
                writeFile (p <.> (getExtension typ ++ maybe "" id (_postfix size))) dat
            return . Right $ p <.> getExtension typ 
    where
    check val fun = either (return . Left) fun val
    rights = fmap (\ (Right v, s) -> (v,s)) . Prelude.filter (isRight . fst)
    isRight (Right _) = True
    isRight _ = False 

decodeBase64 :: Member API r 
             => Text -> Eff r (Either ImageError ByteString)
decodeBase64 dat = do
    let dcd = decode . encodeUtf8 $ dat  
    case dcd of
        Left err -> return . Left . Base64Error $ err
        Right res -> return $ Right res 

-- ATTENTION: If there ever is an error occuring during image scaling,
-- i should tryIOError to blame it on these 'functions' first.

unsafeTry :: (String -> ImageError) -> IO a -> Either ImageError a
unsafeTry err = either (Left . err . show) Right . unsafePerformIO . tryIOError

loadImage :: ImgType -> ByteString -> Either ImageError ProcImage
loadImage typ bs = unsafeTry DecodingError $ case typ of
    PNG -> GD.loadPngByteString bs 
    JPEG -> GD.loadJpegByteString bs 

saveImage :: ImgType -> ProcImage -> Either ImageError ByteString
saveImage typ img = unsafeTry EncodingError $ case typ of
    PNG -> GD.savePngByteString img 
    JPEG -> GD.saveJpegByteString 95 img 

resizeImage :: ProcImage -> ImageSize -> Either ImageError ProcImage 
resizeImage dat s =
    case (_scaleType s) of
        FixedSize -> resizeImageFixed dat (_sizeX s, _sizeY s)
        ScaleToX -> resizeImageToX dat (_sizeX s)
        ScaleToY -> resizeImageToY dat (_sizeY s)

resizeImageFixed :: ProcImage -> (Int, Int) -> Either ImageError ProcImage
resizeImageFixed img to_size@(to_x', to_y') = res
    where
    -- actual size
    (x', y') = unsafePerformIO $ GD.imageSize img 
    (x, y) = (fromIntegral x', fromIntegral y') :: (Float, Float)
    -- desired size
    (to_x, to_y) = (fromIntegral to_x', fromIntegral to_y') :: (Float, Float)
    -- if < 0, then image is portrait
    -- if > 0, then image is landscape
    to_ratio = to_x / to_y
    ratio = x / y
    -- if the desired image is less landscape then
    -- the actual image, we need to crop the
    -- actual image in its width.
    crop_width = to_ratio < ratio 
    -- if we crop width, the scaling factor
    -- is calculated based on the heights,
    -- since we couldd take the complete 
    -- height of the image
    scale = if crop_width
            then y / to_y
            else x / to_x
    -- this is the size of the part we take
    -- from the actual image.
    size_x = scale * to_x;
    size_y = scale * to_y;
    size = (round size_x, round size_y) 
    -- this is the upper left corner of
    -- the part we take from the image
    up_left = ( if crop_width
                then round $ (x - size_x) / 2  
                else 0
              , if crop_width
                then 0
                else round $ (y - size_y) / 2 
              ) 
    res = unsafeTry ResizeError$ do
        new <- transparentImage to_size
        GD.copyRegionScaled (trace (show up_left) up_left) (trace (show size) size) img (0,0) (trace (show to_size) size) new
        return new 

resizeImageToX :: ProcImage -> Int -> Either ImageError ProcImage
resizeImageToX img to_x' = resizeImageFixed img size
    where
    (x', y') = unsafePerformIO $ GD.imageSize img
    (x, y) = (fromIntegral x', fromIntegral y')
    to_x = fromIntegral to_x'
    size = (to_x', round $ to_x * y / x) 

resizeImageToY :: ProcImage -> Int -> Either ImageError ProcImage
resizeImageToY img to_y' = resizeImageFixed img size
    where
    (x', y') = unsafePerformIO $ GD.imageSize img
    (x, y) = (fromIntegral x', fromIntegral y')
    to_y = fromIntegral to_y'
    size = (round $ to_y * x / y, to_y') 

transparentImage :: (Int, Int) -> IO (ProcImage)
transparentImage size = do
    new <- GD.newImage size
    GD.alphaBlending True new
    tr <- GD.colorAllocate 255 255 255 127 new
    GD.fillImage tr new
    GD.saveAlpha True new
    return new
 

