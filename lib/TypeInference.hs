module TypeInference where

import AST
import Store


-- Type definitions

data L1Type = IntType | UnitType | BoolType 
  deriving (Eq, Show)

data LocationType = IntRef

type TypeEnv = Store LocationType


-- Type inference

inferType :: TypeEnv -> Expression -> Maybe L1Type
inferType gamma (Integer n) = Just IntType
inferType gamma (Boolean n) = Just BoolType
inferType gamma (Op e1 op e2) = case (inferType gamma e1, op, inferType gamma e2) of
  (Just IntType, Plus, Just IntType) -> Just IntType
  (Just IntType, GTEQ, Just IntType) -> Just BoolType
  _ -> Nothing
inferType gamma (If e1 e2 e3) = case (inferType gamma e1, inferType gamma e2, inferType gamma e3) of
  (Just BoolType, Just t2, Just t3) | t2 == t3 -> Just t2 
  _ -> Nothing
inferType gamma (Deref l) = case Store.lookup gamma l of 
  Just IntRef -> Just IntType
  _ -> Nothing
inferType gamma (Assign l e) = case (Store.lookup gamma l, inferType gamma e) of
  (Just IntRef, Just IntType) -> Just UnitType
  _ -> Nothing
inferType gamma Skip = Just UnitType
inferType gamma (Seq e1 e2) = case (inferType gamma e1, inferType gamma e2) of 
  (Just UnitType, Just t2) -> Just t2
  _ -> Nothing
inferType gamma (While e1 e2) = case (inferType gamma e1, inferType gamma e2) of
  (Just BoolType, Just UnitType) -> Just UnitType
  _ -> Nothing 


