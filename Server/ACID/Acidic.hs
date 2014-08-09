{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ACID.Acidic
where

import Data.Acid ( makeAcidic )

import ACID.Query
import Model

$(makeAcidic ''NoC  [ 'doLoginQ
                    , 'getOperatorIdQ
                    , 'getChanNameQ
                    , 'getChanDescQ
                    , 'getUserLoginQ
                    , 'getUserNameQ
                    , 'getUserDescQ
                    , 'getUserIconQ
                    , 'getUserOwnedChannelsQ
                    , 'getUserSubscriptionsQ
                    , 'getUserContactsQ
                    , 'getUserByLoginQ
                    , 'messagesQ
                    ])
