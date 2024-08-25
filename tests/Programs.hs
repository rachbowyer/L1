module Programs where

import qualified Data.HashMap.Strict as HashMap

import AST
import Store
import TypeInference


-- Example program to sum a series of numbers

exampleStore :: Store Int
exampleStore = HashMap.fromList [("l1", 3), ("l2", 0)]

exampleStoreTypes :: TypeEnv
exampleStoreTypes = HashMap.fromList [("l1", IntRef), ("l2", IntRef)]

exampleProg :: Expression
exampleProg =  Seq (Assign "l2" (Integer 0))
                   (While (Op (Deref "l1") GTEQ (Integer 1))
                     (Seq (Assign "l2" (Op (Deref "l2") Plus (Deref "l1"))) 
                          (Assign "l1" (Op (Deref "l1") Plus (Integer $ -1)))))
  