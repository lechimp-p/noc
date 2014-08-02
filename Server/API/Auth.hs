{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Auth
where

import Data.Text
import Data.Data (Data, Typeable)
import Data.Time.Clock
import Control.Lens (makeLenses)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Happstack.Server.ClientSession 
        ( ClientSession, emptySession
        , getSession, putSession, expireSession 
        )
import Control.Lens

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


authGet :: (AuthData -> a) -> MonadAPI url AuthData a
authGet f = getSession >>= return . f 

authSet :: (AuthData -> AuthData) -> MonadAPI url AuthData ()
authSet f = getSession >>= putSession . f

authPassword = authGet _password
authLogin = authGet _login
authTimestamp = authGet _timestamp

logUserIn l pw = do
    authSet (set login (Just l)) 
    authSet (set password (Just pw))
