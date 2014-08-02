{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Auth
where

import Data.Text
import Data.Data (Data, Typeable)
import Data.Time.Clock
import Control.Lens (makeLenses)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Happstack.Server.ClientSession 
        ( ClientSession, emptySession, liftSessionStateT )

import API.Monad

data AuthData = AuthData 
    { _login        :: Maybe Text
    , _password     :: Maybe Text
    , _timestamp    :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''AuthData)
makeLenses ''AuthData

instance ClientSession AuthData where
    emptySession = AuthData Nothing Nothing Nothing 

authPassword :: MonadAPI url AuthData Text
authPassword = liftSessionStateT $ view password
