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
                    , 'getChanTypeQ
                    , 'amountOfDistinctUsersQ
                    , 'lastPostTimestampQ
                    , 'getUserLoginQ
                    , 'getUserNameQ
                    , 'getUserDescQ
                    , 'getUserIconQ
                    , 'getUserOwnedChannelsQ
                    , 'getUserSubscriptionsQ
                    , 'getUserContactsQ
                    , 'getUserNotificationsQ
                    , 'getUserByLoginQ
                    , 'messagesQ
                    , 'messagesTillQ
                    , 'doLoginU
                    , 'getOperatorIdU
                    , 'addAdminU
                    , 'rmAdminU
                    , 'getChanNameU 
                    , 'getChanDescU 
                    , 'getChanTypeU
                    , 'setChanNameU
                    , 'setChanDescU
                    , 'setChanTypeU
                    , 'addChanOwnerU
                    , 'addChanProducerU
                    , 'addChanConsumerU
                    , 'rmChanOwnerU
                    , 'rmChanProducerU
                    , 'rmChanConsumerU
                    , 'amountOfDistinctUsersU
                    , 'lastPostTimestampU
                    , 'subscribeToChanU
                    , 'unsubscribeFromChanU
                    , 'getUserLoginU 
                    , 'getUserNameU 
                    , 'getUserDescU 
                    , 'getUserIconU 
                    , 'setUserLoginU
                    , 'setUserPasswordU
                    , 'setUserNameU
                    , 'setUserDescU
                    , 'setUserIconU
                    , 'getUserOwnedChannelsU 
                    , 'getUserSubscriptionsU 
                    , 'getUserContactsU 
                    , 'addUserContactU
                    , 'rmUserContactU
                    , 'getUserNotificationsU
                    , 'addUserNotificationU
                    , 'tryToAddUserNotificationU
                    , 'getUserByLoginU 
                    , 'createUserU
                    , 'createChannelU
                    , 'postU
                    , 'messagesU 
                    , 'messagesTillU
                    ])
