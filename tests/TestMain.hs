module Main where

import qualified System.Exit as Exit
import Test.HUnit

import InterpreterTest
import TypeInferenceTest


tests :: Test
tests = TestList [TestLabel "Interpreter test" interpreterTest,
                  TestLabel "Type inference test" typeInferenceTest] 

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess


-- cabal test to run the tests
