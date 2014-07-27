module Model.Tests
where

import Model
import Distribution.TestSuite

tests :: IO [Tests]
tests = return $ permissionTests ++ functionalityTests

permissionTests = group "Tests of Permissions" $ concat [chanPermTests, userPermTests]
chanPermTests = group "Tests of Permissions on Channels" $
    [
    ]
userPermTests = group "Tests of Permissions on Users." $
    [
    ]

functionalityTests = group "Tests of functionality" $ concat []

-- helpers

group n ts = Group n False ts

test n f action = Test $ TestInstance
    { run = do
        success <- action
        if success 
            then return $ Finished Pass
            else return $ Finished (Fail f) 
    , name = n
    , tags = []
    , options = []
    , setOption = \ _ _ -> Left "There are no options for the Test!"
    }
