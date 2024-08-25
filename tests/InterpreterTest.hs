module InterpreterTest where

import qualified Data.HashMap.Strict as HashMap
import Test.HUnit

import AST
import Interpreter
import Store
import Programs


expected :: (Store Int, Expression)
expected = (HashMap.fromList [("l2",6),("l1",0)],Skip)


interpreterTest :: Test
interpreterTest = TestCase (assertEqual "should return 3"  
                            expected
                            (eval exampleStore $ exampleProg))
 