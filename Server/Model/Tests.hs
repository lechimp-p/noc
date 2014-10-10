{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Model.Tests
where

import Model.Operations
import Model.Simple
import Model.Errors
import Model.BaseTypes 
import Model.Simple.NoC (NoC, mkNoC)

import Distribution.TestSuite hiding (run)
import qualified Distribution.TestSuite as TS
import Control.Applicative ((<$>))
import Data.Text hiding (group)
import qualified Data.Set as S

tests :: IO [Test]
tests = return $ [functionalityTests, permissionTests]

functionalityTests = group "Tests of functionality" $ [nocFuncTests]

nocFuncTests = group "Tests of NoCs functionality."
    [ test "User added." "Could not retreive user after adding."
        $ onFreshNoC $ do
            let name = "xyz"
            uid <- createUser (Login name) (Password name)
            name' <- getUserLogin uid
            return $ (Login name) == name'  
    , test "User added." "Could not retreive user by Login after adding."
        $ onFreshNoC $ do
            let name = "xyz"
            uid <- createUser (Login name) (Password name)
            uid' <- getUserIdByLogin name
            return $ uid == uid'
    , test "Admin added." "User is not an admin after adding."
        $ onFreshNoC $ do
            uid <- createUser (Login "user") (Password "Password") 
            addAdmin uid
            isAdmin uid
    , test "None existing user added as admin." "Could add non existing users as admins."
        $ onFreshNoCFails $ 
            addAdmin (UserId (-1))
    ]


-- Permission Tests

permissionTests = group "Tests of Permissions" $ [nocPermTests, chanPermTests, userPermTests]

nocPermTests = group "Tests of Permissions on NoC"
    [ test "User with id 0 is admin." "Expectation on implementation failed: user with id 0 is not an admin."
        $ onFreshNoC $ do
            isAdmin (UserId 0)
    , test "Admin can run commands on NoC." "Admin can't run commands on NoC."
        $ doesNotFail
            (let l = Login "admin"
                 pw = Password "password"
             in run $ runSimple (mkNoC l pw) Nothing (doLogin l pw >> return ())
            )
    , test "User with wrong password can not run commands on NoC."
           "User with wrong password can run commands on NoC."
        $ let l = Login "admin"
              pw = Password "admin"
              pw' = Password "admin'"
          in failsWith (CantLogin l) 
           $ run $ runSimple (mkNoC l pw) Nothing (doLogin l pw' >> return ())
    , test "User with non existing username can not run commands on NoC."
           "User with non existing username can run commands on NoC."
       $  let l = Login "admin"
              l' = Login "admin'"
              pw = Password "admin"
          in failsWith (CantLogin l') 
           $ run $ runSimple (mkNoC l pw) Nothing (doLogin l' pw >> return ())
    , test "Admin creates a new user." "Admin can not create a new user."
        $ onFreshNoC $ do
            uid <- createUser (Login "admin2") (Password "admin2")
            getUserLogin uid
            return True 
    , test "A non admin creates a new user." "A non admin can create a new user."
        $ onFreshNoCFailsSeq [("owner", "owner", createUser (Login "admin2") (Password "admin2"))]
    , test "Admin adds another admin." "Admin can not add another admin."
        $ onFreshNoC $ do
            uid <- createUser (Login "admin2") (Password "admin2")
            addAdmin uid 
            isAdmin uid 
    , test "Admin removes another admin." "Admin can not remove another admin."
        $ onFreshNoC $ do
            uid <- createUser (Login "admin2") (Password "admin2")
            addAdmin uid 
            rmAdmin uid
            not <$> isAdmin uid
    , test "A new user is no admin." "A new user is an admin."
        $ onFreshNoC $ do
            uid <- createUser (Login "admin2") (Password "admin2")
            not <$> isAdmin uid
    , test "Another user adds admin" "User can add admin despite he is no admin."
        $ onFreshNoCFailsSeq 
            [ ("admin", "admin", createUser (Login "user") (Password "user") >> return ())
            , ("user", "user", getUserIdByLogin "user" >>= addAdmin)
            ]
    , test "Another user removes admin" "User can remove admin despite he is no admin."
        $ onFreshNoCFailsSeq
            [ ("admin", "admin", createUser (Login "user") (Password "user") >> return ())
            , ("user", "user", getUserIdByLogin "admin" >>= rmAdmin)
            ]
    , test "Admin adds another user in two steps." "Assumption that getUserIdByLogin does work fails."
        $ onFreshNoCSeq
            [ ("admin", "admin", createUser (Login "user") (Password "user") >> return True)
            , ("admin", "admin", getUserIdByLogin "user" >>= addAdmin >> return True)
            ]
    ]

chanPermTests = group "Tests of Permissions on Channels" $
    [ test "Check of channel properties." "Assumption on channel properties fail."
        $ onFreshNoCSeq $ mkChannel True ++
            [ ("admin", "admin", do
                oid <- getUserIdByLogin "owner"
                --cid <- Prelude.head . S.toList <$> getUserOwnedChannels oid 
                let isOwner who = getUserIdByLogin who >>= isChanOwner chanId 
                    isProducer who = getUserIdByLogin who >>= isChanProducer chanId 
                    isConsumer who = getUserIdByLogin who >>= isChanConsumer chanId 

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

                return $ and [ oco, ocp, occ, ocn
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
                    [ (pack u, pack u, getUserIdByLogin "not related too" >>= addChanOwner chanId >> return True) ]
            , test (u ++ " adds producer.") (u ++ w ++ "add a producer.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, getUserIdByLogin "not related too" >>= addChanProducer chanId >> return True) ]
            , test (u ++ " adds consumer.") (u ++ w ++ "add a consumer.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, getUserIdByLogin "not related too" >>= addChanConsumer chanId >> return True) ]
            , test (u ++ " removes owner.") (u ++ w ++ "remove an owner.")
                $ t $ mkChannel True ++
                    [ ("admin", "admin", getUserIdByLogin "not related too" >>= addChanOwner chanId >> return True)
                    , (pack u, pack u, getUserIdByLogin "not related too" >>= rmChanOwner chanId >> return True)
                    ]
            , test (u ++ " removes producer.") (u ++ w ++ "remove a producer.")
                $ t $ mkChannel True ++
                    [ ("admin", "admin", getUserIdByLogin "not related too" >>= addChanProducer chanId >> return True)
                    , (pack u, pack u, getUserIdByLogin "not related too" >>= rmChanProducer chanId >> return True)
                    ]
            , test (u ++ " removes consumer.") (u ++ w ++ "remove a consumer.")
                $ t $ mkChannel True ++
                    [ ("admin", "admin", getUserIdByLogin "not related too" >>= addChanConsumer chanId >> return True)
                    , (pack u, pack u, getUserIdByLogin "not related too" >>= rmChanConsumer chanId >> return True)
                    ]
            , test (u ++ " sets name of channel.") (u ++ w ++ "set name of channel.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, setChanName chanId (Name "new name") >> return True) ]
            , test (u ++ " sets description of channel.") (u ++ w ++ "set description of channel.")
                $ t $ mkChannel True ++
                    [ (pack u, pack u, setChanDesc chanId (Desc "new name") >> return True) ]
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

userPermTests = group "Tests of Permissions on Users." $
    -- take 'owner' user as examplet
    ( Prelude.concat . flip fmap [ ("admin", onFreshNoCSeq, " could not")
                                 , ("owner", onFreshNoCSeq, " could not")
                                 , ("producer", onFreshNoCSeq, " could not")
                                 ])
        (\ (u,t,w) -> 
            (flip fmap) [ ("login", \x -> getUserLogin x >> return True)
                        , ("name", \x -> getUserName x >> return True)
                        , ("desc", \x -> getUserDesc x >> return True)
                        , ("icon", \x -> getUserIcon x >> return True)
                        ]
                (\ (n,a) -> 
                    test (u ++ " reads " ++ n ++ " of owner.") (u ++ w ++ " read " ++ n ++ " of owner.")
                        $ t $ mkChannel True ++
                            [ (pack u, pack u, getUserIdByLogin "owner" >>= a ) ] 
                )
        )
    ++ ( Prelude.concat . flip fmap [ ("admin", onFreshNoCSeq, " could not")
                                    , ("owner", onFreshNoCSeq, " could not")
                                    , ("producer", onFreshNoCFailsSeq, " could")
                                    ]) 
        (\ (u,t,w) ->
            (flip fmap) [ ("login", \x -> setUserLogin x (Login "owwwwner") >> return True)
                        , ("name", \x -> setUserName x (Name "the owwwwner") >> return True)
                        , ("desc", \x -> setUserDesc x (Desc "i aarr the owwwner.") >> return True)
                        , ("icon", \x -> setUserIcon x Nothing >> return True)
                        ]
                (\ (n,a) -> 
                  test (u ++ " sets " ++ n ++ " of owner.") (u ++ w ++ " set " ++ n ++ " of owner.")
                    $ t $ mkChannel True ++
                        [ (pack u, pack u, getUserIdByLogin "owner" >>= a) ]
                )
            ++  [ --test (u ++ " reads channels owned by owner.") (u ++ w ++ " read channels owned by owner.")
                  --  $ t $ mkChannel True ++
                  --      [ (pack u, pack u, getUserIdByLogin "owner" >>= getUserOwnedChannels >> return True) ]
                 test (u ++ " reads subscriptions of owner.") (u ++ w ++ " read subscriptions of owner.")
                    $ t $ mkChannel True ++
                        [ (pack u, pack u, getUserIdByLogin "owner" >>= getUserSubscriptions >> return True) ]
                , test (u ++ " reads contacts of owner.") (u ++ w ++ " read contacts of owner.")
                    $ t $ mkChannel True ++
                        [ (pack u, pack u, getUserIdByLogin "owner" >>= getUserContacts >> return True) ] 
                ]
        )

-- helpers
    
mkChannel ret =
    [ ("admin", "admin", do
            createUser (Login "owner") (Password "owner")
            createUser (Login "producer") (Password "producer")
            createUser (Login "consumer") (Password "consumer")
            createUser (Login "not related") (Password "not related")
            createUser (Login "not related too") (Password "not related too")
            return ret 
      )
    , ("owner", "owner", do
            cid <- createChannel (Name "channel")
            getUserIdByLogin "producer" >>= addChanProducer cid
            getUserIdByLogin "consumer" >>= addChanConsumer cid
            return ret
      )
    ]

chanId = ChanId 0


group n ts = Group n False ts

test n f action = Test $ TestInstance
    { TS.run = do
        let success = action
        if success 
            then return $ Finished Pass
            else return $ Finished (Fail f) 
    , name = n
    , tags = []
    , options = []
    , setOption = \ _ _ -> Left "There are no options for the Test!"
    }

doesNotFail :: Either Error (NoC, a) -> Bool
doesNotFail (Left _) = False
doesNotFail (Right _) = True

failsWith :: Error -> Either Error (NoC, a) -> Bool
failsWith _ (Right _) = False
failsWith e (Left e') = e == e' 

onFreshNoC :: Eff (Query :> Update :> Exec :> ()) Bool -> Bool
onFreshNoC op = 
    case run . runSimple (mkNoC (Login "admin") (Password "admin")) (Just (UserId 0)) $ op of
        Left _ -> False
        Right (_, b) -> b 

onFreshNoCSeq :: [(Text, Text, Eff (Query :> Update :> Exec :> ()) Bool)] -> Bool
onFreshNoCSeq ops = runSeq ops (mkNoC (Login "admin") (Password "admin"))
    where 
    runSeq :: [(Text, Text, Eff (Query :> Update :> Exec :> ()) Bool)] -> NoC -> Bool
    runSeq ((l, pw, op):[]) noc = 
        case run . runSimple noc Nothing $ doLogin (Login l) (Password pw) >> op of
            Left _ -> False
            Right (_, b) -> b
    runSeq ((l, pw, op):xs) noc = 
        case run . runSimple noc Nothing $ doLogin (Login l) (Password pw) >> op of
            Left _ -> False
            Right (noc', _) -> runSeq xs noc' 

onFreshNoCFails :: Eff (Query :> Update :> Exec :> ()) a -> Bool
onFreshNoCFails op =
    case run . runSimple (mkNoC (Login "admin") (Password "admin")) (Just (UserId 0)) $ op of
        Left _ -> True
        Right _ -> False

onFreshNoCFailsSeq :: [(Text, Text, Eff (Query :> Update :> Exec :> ()) a)] -> Bool
onFreshNoCFailsSeq ops = runSeq ops (mkNoC (Login "admin") (Password "admin"))
    where
    runSeq ((l, pw, op):[]) noc =
        case run . runSimple noc Nothing $ doLogin (Login l) (Password pw) >> op of
            Left _ -> True
            otherwise -> False
    runSeq ((l, pw, op):xs) noc =
        case run . runSimple noc Nothing $ doLogin (Login l) (Password pw) >> op of
            Left _ -> False
            Right (noc', _) -> runSeq xs noc'
