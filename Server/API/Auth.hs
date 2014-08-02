module API.Auth
where

import Data.Text
import Data.Data (Data, Typeable)
import Data.Time.Clock
import Control.Lens (makeLenses)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

data AuthData = AuthData 
    { _login        = Maybe Text
    , _password     = Maybe Text
    , _timestamp    = Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''AuthData)
makeLenses ''AuthData

instance ClientSession AuthData
    emptySession = AuthData Nothing Nothing Nothing 
    
