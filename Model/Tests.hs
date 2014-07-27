{-# LANGUAGE OverloadedStrings #-}

module Model.Tests
where

import Control.Applicative ((<$>))
import Data.Text hiding (group)
import qualified Data.Set as S

import Model
import Model.BaseTypes (UserId (..), ChanId (..), MsgId (..), Login, Password)
import Model.NoC (NoC)
import Model.Permissions (isNoCAdmin, isAdminOf, isOwnerOf, isProducerIn, isConsumerIn)
import Distribution.TestSuite

tests :: IO [Test]
tests = return $ [functionalityTests, permissionTests]

functionalityTests = group "Tests of functionality" $ [nocFuncTests]

nocFuncTests = group "Tests of NoCs functionality."
    [ test "User added." "Could not retreive user after adding."
        $ onFreshNoC $ do
            let name = "xyz"
            uid <- createUser (mkLogin name) (mkPassword name)
            name' <- getUserLogin uid
            return $ (mkLogin name) == name'  
    , test "User added." "Could not retreive user by login after adding."
        $ onFreshNoC $ do
            let name = "xyz"
            uid <- createUser (mkLogin name) (mkPassword name)
            uid' <- getUserByLogin name
            return $ uid == uid'
    , test "Admin added." "User is not an admin after adding."
        $ onFreshNoC $ do
            uid <- createUser (mkLogin "user") (mkPassword "password") 
            addAdmin uid
            isNoCAdmin uid
    , test "None existing user added as admin." "Could add non existing users as admins."
        $ onFreshNoCFails $ 
            addAdmin (UserId (-1))
    ]

permissionTests = group "Tests of Permissions" $ [nocPermTests, chanPermTests, userPermTests]

nocPermTests = group "Tests of Permissions on NoC"
    [ test "User with id 0 is admin." "Expectation on implementation failed: user with id 0 is not an admin."
        $ onFreshNoC $ do
            isNoCAdmin (UserId 0)
    , test "Admin creates a new user." "Admin can not create a new user."
        $ onFreshNoC $ do
            uid <- createUser (mkLogin "admin2") (mkPassword "admin2")
            getUserLogin uid
            return True 
    , test "Admin adds another admin." "Admin can not add another admin."
        $ onFreshNoC $ do
            uid <- createUser (mkLogin "admin2") (mkPassword "admin2")
            addAdmin uid 
            isNoCAdmin uid 
    , test "Admin removes another admin." "Admin can not remove another admin."
        $ onFreshNoC $ do
            uid <- createUser (mkLogin "admin2") (mkPassword "admin2")
            addAdmin uid 
            rmAdmin uid
            not <$> isNoCAdmin uid
    , test "A new user is no admin." "A new user is an admin."
        $ onFreshNoC $ do
            uid <- createUser (mkLogin "admin2") (mkPassword "admin2")
            not <$> isNoCAdmin uid
    , test "Another user adds admin" "User can add admin despite he is no admin."
        $ onFreshNoCFailsSeq 
            [ ("admin", "admin", createUser (mkLogin "user") (mkPassword "user") >> return ())
            , ("user", "user", getUserByLogin "user" >>= addAdmin)
            ]
    , test "Another user removes admin" "User can remove admin despite he is no admin."
        $ onFreshNoCFailsSeq
            [ ("admin", "admin", createUser (mkLogin "user") (mkPassword "user") >> return ())
            , ("user", "user", getUserByLogin "admin" >>= rmAdmin)
            ]
    , test "Admin adds another user in two steps." "Assumption that getUserByLogin does work fails."
        $ onFreshNoCSeq
            [ ("admin", "admin", createUser (mkLogin "user") (mkPassword "user") >> return True)
            , ("admin", "admin", getUserByLogin "user" >>= addAdmin >> return True)
            ]
    ]
    
mkChannel ret =
    [ ("admin", "admin", do
            createUser (mkLogin "owner") (mkPassword "owner")
            createUser (mkLogin "producer") (mkPassword "producer")
            createUser (mkLogin "consumer") (mkPassword "consumer")
            createUser (mkLogin "not related") (mkPassword "not related")
            createUser (mkLogin "not related too") (mkPassword "not related too")
            return ret 
      )
    , ("owner", "owner", do
            cid <- createChannel (mkName "channel") (mkDesc "channel description")
            getUserByLogin "producer" >>= addChanProducer cid
            getUserByLogin "consumer" >>= addChanConsumer cid
            return ret
      )
    ]

chanId = ChanId 0

chanPermTests = group "Tests of Permissions on Channels" $
    [ test "Check of channel properties." "Assumption on channel properties fail."
        $ onFreshNoCSeq $ mkChannel True ++
            [ ("admin", "admin", do
                oid <- getUserByLogin "owner"
                cid <- Prelude.head . S.toList <$> getUserOwnedChannels oid 
                let isOwner who = getUserByLogin who >>= flip isOwnerOf cid
                    isProducer who = getUserByLogin who >>= flip isProducerIn cid
                    isConsumer who = getUserByLogin who >>= flip isConsumerIn cid

                oco <- isOwner "owner"
                ocp <- not <$> isOwner "producer"
                occ <- not <$> isOwner "consumer"
                ocn <- not <$> isOwner "not related"

                pco <- not <$> isProducer "owner"
                pcp <- isProducer "producer"
                pcc <- not <$> isProducer "consumer"
                pcn <- not <$> isProducer "not related"

                cco <- not <$> isConsumer "owner"
                ccp <- not <$> isConsumer "producer"
                ccc <- isConsumer "consumer"
                ccn <- not <$> isConsumer "not related"

                return $ and [ cid == chanId 
                             , oco, ocp, occ, ocn
                             , pco, pcp, pcc, pcn
                             , cco, ccp, ccc, ccn
                             ]
               )
            ]
    ]
    ++ (Prelude.concat . flip fmap [ ("admin", onFreshNoCSeq, " could not ")
                                   , ("owner", onFreshNoCSeq, " could not ")
                                   , ("producer", onFreshNoCFailsSeq, " could ")
                                   , ("consumer", onFreshNoCFailsSeq, " could ")
                                   , ("not related", onFreshNoCFailsSeq, " could ")
                                   ]) 
        (\ (u,t,w) -> 
            [ test (u ++ " adds owner.") (u ++ w ++ "add an owner.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, getUserByLogin "not related too" >>= addChanOwner chanId >> return True) ]
            , test (u ++ " adds producer.") (u ++ w ++ "add a producer.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, getUserByLogin "not related too" >>= addChanProducer chanId >> return True) ]
            , test (u ++ " adds consumer.") (u ++ w ++ "add a consumer.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, getUserByLogin "not related too" >>= addChanConsumer chanId >> return True) ]
            , test (u ++ " removes owner.") (u ++ w ++ "remove an owner.")
                $ t $ mkChannel True ++
                    [ ("admin", "admin", getUserByLogin "not related too" >>= addChanOwner chanId >> return True)
                    , (pack u, pack u, getUserByLogin "not related too" >>= rmChanOwner chanId >> return True)
                    ]
            , test (u ++ " removes producer.") (u ++ w ++ "remove a producer.")
                $ t $ mkChannel True ++
                    [ ("admin", "admin", getUserByLogin "not related too" >>= addChanProducer chanId >> return True)
                    , (pack u, pack u, getUserByLogin "not related too" >>= rmChanProducer chanId >> return True)
                    ]
            , test (u ++ " removes consumer.") (u ++ w ++ "remove a consumer.")
                $ t $ mkChannel True ++
                    [ ("admin", "admin", getUserByLogin "not related too" >>= addChanConsumer chanId >> return True)
                    , (pack u, pack u, getUserByLogin "not related too" >>= rmChanConsumer chanId >> return True)
                    ]
            , test (u ++ " sets name of channel.") (u ++ w ++ "set name of channel.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, setChanName chanId (mkName "new name") >> return True) ]
            , test (u ++ " sets description of channel.") (u ++ w ++ "set description of channel.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, setChanDesc chanId (mkDesc "new name") >> return True) ]
            ]
        )
    ++ (Prelude.concat . flip fmap [ ("admin", onFreshNoCSeq, " could not ")
                                   , ("owner", onFreshNoCSeq, " could not ")
                                   , ("producer", onFreshNoCSeq, " could not ")
                                   , ("consumer", onFreshNoCSeq, " could not ")
                                   , ("not related", onFreshNoCFailsSeq, " could ")
                                   ]) 
        (\ (u,t,w) -> 
            [ test (u ++ " reads name of channel.") (u ++ w ++ " read name of channel.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, getChanName chanId >> return True) ]
            , test (u ++ " reads description of channel.") (u ++ w ++ " read description of channel.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, getChanDesc chanId >> return True) ]
            ]
        )

userPermTests = group "Tests of Permissions on Users."
    [
    ]


-- helpers

group n ts = Group n False ts

test n f action = Test $ TestInstance
    { run = do
        let success = action
        if success 
            then return $ Finished Pass
            else return $ Finished (Fail f) 
    , name = n
    , tags = []
    , options = []
    , setOption = \ _ _ -> Left "There are no options for the Test!"
    }


onFreshNoC :: Operation Bool -> Bool
onFreshNoC op = 
    case runOp (mkNoC l pw) l pw op of
        Left _ -> False
        Right (_, b) -> b 
    where
    l = mkLogin "admin"
    pw = mkPassword "admin"

onFreshNoCSeq :: [(Text, Text, Operation Bool)] -> Bool
onFreshNoCSeq ops = runOpSeq ops (mkNoC (mkLogin "admin") (mkPassword "admin"))
    where 
    runOpSeq :: [(Text, Text, Operation Bool)] -> NoC -> Bool
    runOpSeq ((l, pw, op):[]) noc = 
        case runOp noc (mkLogin l) (mkPassword pw) op of
            Left _ -> False
            Right (_, b) -> b
    runOpSeq ((l, pw, op):xs) noc = 
        case runOp noc (mkLogin l) (mkPassword pw) op of
            Left _ -> False
            Right (noc', _) -> runOpSeq xs noc' 

onFreshNoCFails :: Operation a -> Bool
onFreshNoCFails op =
    case runOp (mkNoC l pw) l pw op of
        Left _ -> True
        Right _ -> False
    where
    l = mkLogin "admin"
    pw = mkPassword "admin"

onFreshNoCFailsSeq :: [(Text, Text, Operation a)] -> Bool
onFreshNoCFailsSeq ops = runOpSeq ops (mkNoC (mkLogin "admin") (mkPassword "admin"))
    where
    runOpSeq ((l, pw, op):[]) noc =
        case runOp noc (mkLogin l) (mkPassword pw) op of
            Left _ -> True
            otherwise -> False
    runOpSeq ((l, pw, op):xs) noc =
        case runOp noc (mkLogin l) (mkPassword pw) op of
            Left _ -> False
            Right (noc', _) -> runOpSeq xs noc'
