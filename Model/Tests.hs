{-# LANGUAGE OverloadedStrings #-}

module Model.Tests
where

import Model
import Distribution.TestSuite

tests :: IO [Test]
tests = return $ [functionalityTests, permissionTests]

functionalityTests = group "Tests of functionality" $ [nocFuncTests]

nocFuncTests = group "Tests of NoCs functionality."
    [ 
    ]

permissionTests = group "Tests of Permissions" $ [chanPermTests, userPermTests]
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
    case runOp (mkNoC (mkLogin "admin") (mkPassword "admin")) (mkLogin "admin") (mkPassword "admin") op of
        Left _ -> False
        Right (_, b) -> b 

