{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ACID.Acidic
where

import Data.Acid ( makeAcidic )

import ACID.Query
import ACID.Update
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
                    , 'doLoginU
                    , 'getOperatorIdU
                    , 'addAdminU
                    , 'rmAdminU
                    , 'getChanNameU 
                    , 'getChanDescU 
                    , 'setChanNameU
                    , 'setChanDescU
                    , 'addChanOwnerU
                    , 'addChanProducerU
                    , 'addChanConsumerU
                    , 'rmChanOwnerU
                    , 'rmChanProducerU
                    , 'rmChanConsumerU
                    , 'getUserLoginU 
                    , 'getUserNameU 
                    , 'getUserDescU 
                    , 'getUserIconU 
                    , 'setUserLoginU
                    , 'setUserNameU
                    , 'setUserDescU
                    , 'setUserIconU
                    , 'getUserOwnedChannelsU 
                    , 'getUserSubscriptionsU 
                    , 'getUserContactsU 
                    , 'getUserByLoginU 
                    , 'createUserU
                    , 'createChannelU
                    , 'postU
                    , 'messagesU 
                    ])
