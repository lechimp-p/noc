{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Simple.User
where

import Data.Data (Data, Typeable) 
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens (makeLenses)
import Data.Time.Clock
import Data.IxSet (Indexable, empty, IxSet, ixSet, ixFun) 
import Text.Email.Validate (EmailAddress)

import Model.BaseTypes

data User = User
    { _id            :: UserId
    , _login         :: Login
    , _password      :: Password
    , _name          :: Name
    , _desc          :: Desc
    , _icon          :: Maybe Icon
    , _email         :: Maybe EmailAddress
    , _ownedChannels :: S.Set ChanId
    , _subscriptions :: S.Set ChanId
    , _contacts      :: M.Map UserId Contact 
    , _notifications :: [Notification]
    }
    deriving (Data, Typeable)

instance Eq User where
    u == u' = _id u == _id u'

instance Ord User where
    compare u u' = compare (_id u) (_id u')

makeLenses ''User

-- an index for search on user login
newtype IxLoginSearch = IxLoginSearch Text
                      deriving (Eq, Ord, Data, Typeable)

-- An index for words in the name.
-- newtype IxName = IxName Text 
--                 deriving (Eq, Ord, Data, Typeable)
-- An index for words an heads of words in the name
-- newtype IxAutoComplete = IxAutoComplete Text
--                 deriving (Eq, Ord, Data, Typeable)
-- An index for words in the description
-- newtype IxDesc = IxDesc Text
--                 deriving (Eq, Ord, Data, Typeable)
                  
instance Indexable User where
    empty = ixSet
        [ ixFun $ (:[]) . _id
        , ixFun $ (:[]) . _login
        , ixFun $ fmap   ( IxLoginSearch . T.reverse )
                . filter ( not . T.null )
                . concat
                . fmap   ( T.tails . T.reverse )
                . T.words . loginToText . _login
--        , ixFun $ fmap ( IxAutoComplete . T.reverse )  
--                . filter (not . T.null)
--                . concat
--                . fmap T.tails
--                . T.words . T.reverse . nameToText . _name 
--        , ixFun $ fmap IxName . T.words . nameToText . _name
--        , ixFun $ fmap IxDesc . T.words . descToText . _desc 
        ]

-- Old datatypes for migrations

data User0 = User0
    { _id0            :: UserId
    , _login0         :: Login
    , _password0      :: Password
    , _name0          :: Name
    , _desc0          :: Desc
    , _icon0          :: Maybe Icon
    , _email0         :: Maybe EmailAddress
    , _ownedChannels0 :: S.Set ChanId
    , _subscriptions0 :: S.Set ChanId
    , _contacts0      :: S.Set UserId
    , _notifications0 :: [Notification]
    }
    deriving (Data, Typeable)

instance Eq User0 where
    u == u' = _id0 u == _id0 u'

instance Ord User0 where
    compare u u' = compare (_id0 u) (_id0 u')

data User1 = User1
    { _id1            :: UserId
    , _login1         :: Login
    , _password1      :: Password
    , _name1          :: Name
    , _desc1          :: Desc
    , _icon1          :: Maybe Icon
    , _email1         :: Maybe EmailAddress
    , _ownedChannels1 :: S.Set ChanId
    , _subscriptions1 :: S.Set ChanId
    , _contacts1      :: S.Set Contact 
    , _notifications1 :: [Notification]
    }
    deriving (Data, Typeable)

instance Eq User1 where
    u == u' = _id1 u == _id1 u'

instance Ord User1 where
    compare u u' = compare (_id1 u) (_id1 u')


