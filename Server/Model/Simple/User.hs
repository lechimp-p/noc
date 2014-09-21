{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Simple.User
where

import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens (makeLenses)
import Data.Time.Clock
import Data.IxSet (Indexable, empty, IxSet, ixSet, ixFun) 

import Model.BaseTypes

data User = User
    { _id            :: UserId
    , _login         :: Login
    , _password      :: Password
    , _name          :: Name
    , _desc          :: Desc
    , _icon          :: Maybe Icon
    , _ownedChannels :: S.Set ChanId
    , _subscriptions :: S.Set ChanId
    , _contacts      :: S.Set UserId
    , _notifications :: [Notification]
    }
    deriving (Data, Typeable)

instance Eq User where
    u == u' = _id u == _id u'

instance Ord User where
    compare u u' = compare (_id u) (_id u')

makeLenses ''User

-- An index for words in the name.
newtype IxName = IxName Text 
                 deriving (Eq, Ord, Data, Typeable)
-- An index for words an heads of words in the name
newtype IxAutoComplete = IxAutoComplete Text
                 deriving (Eq, Ord, Data, Typeable)
-- An index for words in the description
newtype IxDesc = IxDesc Text
                 deriving (Eq, Ord, Data, Typeable)
                  
instance Indexable User where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . _login
        , ixFun $ fmap ( IxAutoComplete . T.reverse )  
                . filter (not . T.null)
                . concat
                . fmap T.tails
                . T.words . T.reverse . nameToText . _name 
        , ixFun $ fmap IxName . T.words . nameToText . _name
        , ixFun $ fmap IxDesc . T.words . descToText . _desc 
        ]
