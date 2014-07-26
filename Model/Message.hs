{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Message 
where

import Prelude hiding (words)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text 
import Data.Time.Clock (UTCTime)
import Data.IxSet (Indexable, empty, ixSet, ixFun) 
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

import Model.BaseTypes

data Message = Message 
    { _id           :: MsgId
    , _image        :: Image
    , _text         :: Text
    , _author       :: UserId
    , _timestamp    :: UTCTime
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Message) 

-- An index for words in the text of the message.
newtype IxText = IxText Text 
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
                  
instance Indexable Message where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . _author
        , ixFun $ (:[]) . _timestamp
        , ixFun $ fmap IxText . words . _text
        ]


