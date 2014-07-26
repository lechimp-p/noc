{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User
where

import Prelude hiding (words)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text 
import Data.IxSet (Indexable, empty, ixSet, ixFun) 
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

import Model.BaseTypes

data User = User
    { _id            :: UserId
    , _login         :: Login
    , _password      :: Password
    , _name          :: Name
    , _desc          :: Desc
    , _icon          :: Icon
    , _ownedChannels :: S.Set [ChanId]
    , _subscriptions :: S.Set [ChanId]
    , _contacts      :: S.Set [UserId]
    }
    deriving (Data, Typeable)

instance Eq User where
    u == u' = _id u == _id u'

instance Ord User where
    compare u u' = compare (_id u) (_id u')

$(deriveSafeCopy 0 'base ''User) 

-- An index for words in the name.
newtype IxName = IxName Text 
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- An index for word in the description
newtype IxDesc = IxDesc Text
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
                  
instance Indexable User where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . _login
        , ixFun $ fmap IxName . words . nameToText . _name
        , ixFun $ fmap IxDesc . words . descToText . _desc 
        ]


