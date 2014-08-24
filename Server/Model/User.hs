{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User
where

import Prelude hiding (null, words, reverse)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text hiding (concat, filter)
import Data.IxSet (Indexable, empty, ixSet, ixFun) 
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Control.Lens (makeLenses)

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

data Notification
    = AddedToChannel UserId ChanId
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Notification)
$(deriveSafeCopy 0 'base ''User) 
makeLenses ''User

-- An index for words in the name.
newtype IxName = IxName Text 
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- An index for words an heads of words in the name
newtype IxAutoComplete = IxAutoComplete Text
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- An index for words in the description
newtype IxDesc = IxDesc Text
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
                  
instance Indexable User where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . _login
        , ixFun $ fmap ( IxAutoComplete . reverse )  
                . filter (not . null)
                . concat
                . fmap tails
                . words . reverse . nameToText . _name 
        , ixFun $ fmap IxName . words . nameToText . _name
        , ixFun $ fmap IxDesc . words . descToText . _desc 
        ]


