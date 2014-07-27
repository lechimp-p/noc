{-# LANGUAGE OverloadedStrings #-}

module Model.Tests
where

import Control.Applicative ((<$>))
import Data.Text hiding (group)

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
            uid <- createUser (mkLogin name) (mkPassword "password")
            name' <- getUserLogin uid
            return $ (mkLogin name) == name'  
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
    --, test "Another user removes admin" "User can remove admin despite he is no admin."
    ]
    

chanPermTests = group "Tests of Permissions on Channels"
    [
    ]
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

onFreshNoCFailsSeq :: [(Text, Text, Operation ())] -> Bool
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
