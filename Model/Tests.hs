module Model.Tests
where

import Model
import Distribution.TestSuite

tests :: IO [Tests]
tests = return $ functionalityTests ++ permissionTests

functionalityTests = group "Tests of functionality" $ concat []

permissionTests = group "Tests of Permissions" $ concat [chanPermTests, userPermTests]
chanPermTests = group "Tests of Permissions on Channels" $
    [
    ]
userPermTests = group "Tests of Permissions on Users." $
    [
    ]


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
