{-# LANGUAGE DeriveDataTypeable #-}

module Model.Simple.Message
    ( module Model.Message
    , module Model.Simple.Message 
    )
where

import Model.Message

import Data.Data (Data, Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.IxSet (Indexable, empty, IxSet, ixSet, ixFun) 

-- An index for words in the text of the message.
newtype IxText = IxText Text 
                 deriving (Eq, Ord, Data, Typeable)
                  
instance Indexable Message where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . _chan
        , ixFun $ (:[]) . _author
        , ixFun $ (:[]) . _timestamp
        , ixFun $ fmap IxText . T.words . _text
        ]
