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
import Happstack.Server
        ( Response )
import Happstack.Server.ClientSession 
        ( ClientSession, emptySession
        , getSession, putSession, expireSession 
        )
import Control.Lens
import Data.Acid.Advanced ( query' )

import API.ACIDEvents
import API.Monad
import API.Utils
import API.Errors
import Model
import Model.Errors

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


authGet :: (AuthData -> a) -> APIMonad url AuthData a
authGet f = getSession >>= return . f 

authSet :: (AuthData -> AuthData) -> APIMonad url AuthData ()
authSet f = getSession >>= putSession . f

authPassword = authGet _password
authLogin = authGet _login
authTimestamp = authGet _timestamp

logUserIn :: ACID -> Text -> Text -> APIMonad url AuthData Response
logUserIn acid l pw = 
    let ta = QueryTA $ LoginTA (mkLogin l) (mkPassword pw)  
    in handleErrors acid ta $ \ _ -> do
        authSet (set login (Just l)) 
        authSet (set password (Just pw))
        noContent'

logUserOut :: APIMonad url AuthData Response
logUserOut = do
    authSet (set login Nothing)
    authSet (set password Nothing)
    noContent'

withAuth :: (Login -> Password -> Transaction (Either Error a) NoC)
         -> APIMonad url AuthData (Maybe (Transaction (Either Error a) NoC))
withAuth tr = do
    l <- authGet _login
    pw <- authGet _password
    case (l, pw) of
        (Just l', Just pw') -> return . Just $ tr (mkLogin l') (mkPassword pw')
        otherwise           -> return Nothing 

runHandler :: ACID
           -> (Login -> Password -> Transaction (Either Error a) NoC)
           -> (a -> APIMonad url AuthData Response)
           -> APIMonad url AuthData Response
runHandler acid tr op = do
    ta <- withAuth tr
    case ta of
        (Just ta')  -> handleErrors acid ta' op 
        Nothing     -> respondError NotLoggedIn  

