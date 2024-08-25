module TypeInferenceTest where

import Test.HUnit

import Programs
import TypeInference

typeInferenceTest :: Test
typeInferenceTest = TestCase (assertEqual "Program type checks"  
                             (Just UnitType)
                             (inferType exampleStoreTypes exampleProg))