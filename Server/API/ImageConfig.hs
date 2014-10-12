{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module API.ImageConfig where

import Data.Data (Data, Typeable)
import Control.Lens (makeLenses)

data ImageConfig = ImageConfig
    { _userIconDir          :: FilePath
    , _userIconSizes        :: [ImageSize]
    , _channelImageDir      :: FilePath
    , _channelImageSizes    :: [ImageSize]
    , _msgImageDir          :: FilePath
    , _msgImageSizes        :: [ImageSize]
    }
    deriving (Show, Data, Typeable)

data ScaleType
    = FixedSize
    | ScaleToX
    | ScaleToY
    deriving (Show, Data, Typeable)

data ImageSize = ImageSize
    { _postfix              :: Maybe String
    , _sizeX                :: Int
    , _sizeY                :: Int
    , _scaleType            :: ScaleType
    }
    deriving (Show, Data, Typeable)

makeLenses ''ImageSize
makeLenses ''ImageConfig

