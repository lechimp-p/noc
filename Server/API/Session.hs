{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module API.Session where

import Model.BaseTypes

import Data.Time.Clock (UTCTime)
import Data.Data (Typeable)
import Control.Lens (makeLenses)

data AuthData = AuthData 
    { _login        :: Maybe Login 
    , _password     :: Maybe Password 
    , _timestamp    :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Typeable)

makeLenses ''AuthData


