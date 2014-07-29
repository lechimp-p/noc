module Tests
    ( tests
    )
where

import Distribution.TestSuite

import qualified Model.Tests as ModelTests

tests :: IO [Test]
tests = ModelTests.tests 
