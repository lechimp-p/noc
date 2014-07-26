{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Channel
where

import Prelude hiding (words)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text 
import Data.IxSet (Indexable, empty, ixSet, ixFun) 
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

import Model.BaseTypes

data Channel = Channel
    { _id           :: ChanId
    , _name         :: Name
    , _desc         :: Desc
    , _owners       :: S.Set UserId
    , _producers    :: S.Set UserId
    , _consumers    :: S.Set UserId
    , _messages     :: S.Set MsgId
    } 
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Channel) 

-- An index for words in the name.
newtype IxName = IxName Text 
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- An index for word in the description
newtype IxDesc = IxDesc Text
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
                  
instance Indexable Channel where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ fmap IxName . words . nameToText . _name
        , ixFun $ fmap IxDesc . words . descToText . _desc 
        ]
