module Main where

import qualified Data.HashMap.Strict as HashMap

import AST
import Interpreter
import Store
import TypeInference



-- Example Program - sums a sequence of numbers

exampleStore :: Store Int
exampleStore = HashMap.fromList [("l1", 3), ("l2", 0)]

exampleStoreTypes :: TypeEnv
exampleStoreTypes = HashMap.fromList [("l1", IntRef), ("l2", IntRef)]

exampleProg :: Expression
exampleProg =  Seq (Assign "l2" (Integer 0))
                   (While (Op (Deref "l1") GTEQ (Integer 1))
                     (Seq (Assign "l2" (Op (Deref "l2") Plus (Deref "l1"))) 
                          (Assign "l1" (Op (Deref "l1") Plus (Integer $ -1)))))
  

main :: IO ()
main = let results = show $ eval exampleStore exampleProg
           types = show $ inferType exampleStoreTypes exampleProg
         in putStrLn (types ++ " " ++ results)

-- cabal run
