{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Channel
where

import Prelude hiding (null, words, reverse)
import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text hiding (concat, filter)
import Data.IxSet (Indexable, empty, ixSet, ixFun) 
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Control.Lens (makeLenses)

import Model.BaseTypes

data Channel = Channel
    { _id           :: ChanId
    , _name         :: Name
    , _desc         :: Desc
    , _type'        :: ChanType
    , _owners       :: S.Set UserId
    , _producers    :: S.Set UserId
    , _consumers    :: S.Set UserId
    , _messages     :: S.Set MsgId
    } 
    deriving (Data, Typeable)

instance Eq Channel where
    c == c' = _id c == _id c'

instance Ord Channel where
    compare c c' = compare (_id c) (_id c') 

data ChanType
    = None
    | Stream
    | Conversation
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''ChanType)
$(deriveSafeCopy 0 'base ''Channel) 
makeLenses ''Channel



newtype IxExactName = IxExactName Text
                      deriving (Eq, Ord, Data, Typeable, SafeCopy) 
-- An index for words in the name.
newtype IxName = IxName Text 
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- An index for autocompletion of the name.
newtype IxAutoComplete = IxAutoComplete Text
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- An index for word in the description
newtype IxDesc = IxDesc Text
                 deriving (Eq, Ord, Data, Typeable, SafeCopy)
                  
instance Indexable Channel where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . nameToText . _name
        , ixFun $ fmap IxName . words . nameToText . _name
        , ixFun $ fmap ( IxAutoComplete . reverse )  
                . filter (not . null)
                . concat
                . fmap tails
                . words . reverse . nameToText . _name
        , ixFun $ fmap IxDesc . words . descToText . _desc 
        ]
