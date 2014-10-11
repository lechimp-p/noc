{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Config
    ( module API.Config
    , module API.ImageConfig
    )
where

import API.ImageConfig

import Data.Text
import Data.Int
import Data.Data (Data, Typeable)
import Control.Lens (makeLenses)
import Control.Lens.Getter (Getting)
import Happstack.Server.Internal.Cookie

data APIConfig = APIConfig
    { _helloWorldMessage    :: Text 
    , _sessionConfig        :: SessionConfig
    , _siteConfig           :: SiteConfig
    , _filesPath            :: FilePath 
    , _acidPath             :: FilePath 
    , _imageConfig          :: ImageConfig
    , _bodyPolicy           :: BodyPolicy
    , _serverConfig         :: ServerConfig
    }
    deriving (Show, Data, Typeable)

data SessionConfig = SessionConfig
    { _cookieName           :: String
    , _cookieLifetime       :: CookieLife
    , _cookieDomain         :: String
    , _cookiePath           :: String
    , _keyfileName          :: FilePath 
    , _httpsOnly            :: Bool
    }
    deriving (Show, Data, Typeable)

deriving instance Data CookieLife

data SiteConfig = SiteConfig
    { _location             :: Text
    , _handlerPath          :: Text 
    } 
    deriving (Show, Data, Typeable)
data BodyPolicy = BodyPolicy
    { _uploadPath           :: FilePath
    , _maxBytesFile         :: Int64
    , _maxBytesBody         :: Int64
    , _maxBytesHeader       :: Int64
    }
    deriving (Show, Data, Typeable)

data ServerConfig = ServerConfig
    { _numThreads           :: Int
    , _threadTimeout        :: Int
    , _port                 :: Int
    }
    deriving (Show, Data, Typeable)

makeLenses ''APIConfig
makeLenses ''SessionConfig
makeLenses ''SiteConfig
makeLenses ''BodyPolicy
makeLenses ''ServerConfig
