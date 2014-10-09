{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Acid.Acidic where

import Model.Simple.NoC (NoC)

import Data.Acid (makeAcidic)
import Model.Acid.Query
import Model.Acid.Update

$(makeAcidic ''NoC  [ 'qDoLogin
                    , 'qIsAdmin
                    , 'qCountAdmins
                    , 'qGetUserIdByLogin
                    , 'qSearchUserByLogin
                    , 'qGetChanName
                    , 'qGetChanDesc
                    , 'qGetChanType
                    , 'qGetChanImage
                    , 'qIsChanOwner
                    , 'qIsChanProducer
                    , 'qIsChanConsumer
                    , 'qAmountOfSubscribedUsers
                    , 'qLastPostTimestamp
                    , 'qMessages
                    , 'qMessagesTill
                    , 'qGetUserLogin
                    , 'qGetUserName
                    , 'qGetUserDesc
                    , 'qGetUserIcon
                    , 'qGetUserNotifications
                    , 'qGetUserContacts
                    , 'qGetUserSubscriptions
                    , 'uCreateChan
                    , 'uCreateUser
                    , 'uAddAdmin
                    , 'uRmAdmin 
                    , 'uSetChanName
                    , 'uSetChanDesc
                    , 'uSetChanType
                    , 'uSetChanImage
                    , 'uAddChanOwner
                    , 'uRmChanOwner
                    , 'uAddChanProducer
                    , 'uRmChanProducer
                    , 'uAddChanConsumer
                    , 'uRmChanConsumer
                    , 'uPost
                    , 'uSetUserLogin
                    , 'uSetUserPassword
                    , 'uSetUserName
                    , 'uSetUserDesc
                    , 'uSetUserIcon
                    , 'uAddUserNotification
                    , 'uAddUserContact
                    , 'uRmUserContact
                    , 'uAddUserSubscription
                    , 'uRmUserSubscription
                    ])

