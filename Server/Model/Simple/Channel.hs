{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Simple.Channel
where

import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens (makeLenses)
import Data.IxSet (Indexable, empty, IxSet, ixSet, ixFun) 

import Model.BaseTypes

data Channel = Channel
    { _id           :: ChanId
    , _name         :: Name
    , _desc         :: Desc
    , _type'        :: ChanType
    , _image        :: Maybe Image
    , _owners       :: S.Set UserId
    , _producers    :: S.Set UserId
    , _consumers    :: S.Set UserId
    , _subscribers  :: S.Set UserId
    , _messages     :: S.Set MsgId
    } 
    deriving (Data, Typeable)

instance Eq Channel where
    c == c' = _id c == _id c'

instance Ord Channel where
    compare c c' = compare (_id c) (_id c') 

makeLenses ''Channel

--newtype IxExactName = IxExactName Text
--                      deriving (Eq, Ord, Data, Typeable) 
-- An index for words in the name.
--newtype IxName = IxName Text 
--                 deriving (Eq, Ord, Data, Typeable)
-- An index for autocompletion of the name.
newtype IxChanNameSearch = IxChanNameSearch Text
                 deriving (Eq, Ord, Data, Typeable)
-- An index for word in the description
--newtype IxDesc = IxDesc Text
--                 deriving (Eq, Ord, Data, Typeable)
                  
instance Indexable Channel where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . nameToText . _name
--        , ixFun $ fmap IxName . T.words . nameToText . _name
        , ixFun $ fmap ( IxChanNameSearch . T.reverse )  
                . filter (not . T.null)
                . concat
                . fmap T.tails
                . T.words . T.reverse . nameToText . _name
--        , ixFun $ fmap IxDesc . T.words . descToText . _desc 
        ]
