module Interpreter where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import AST
import Store


-- Evaluator

isValue :: Expression -> Bool
isValue (Integer _) = True
isValue (Boolean _) = True
isValue Skip = True
isValue _ = False

                                                   
eval1 :: Store Int -> Expression -> Maybe (Store Int, Expression)
eval1 _ (Integer _) = Nothing
eval1 _ (Boolean _) = Nothing
eval1 _ Skip = Nothing
eval1 store (If (Boolean True) e2 _) = Just (store, e2)
eval1 store (If (Boolean False) _ e3) = Just (store, e3)
eval1 store (If e1 e2 e3) = fmap (\(store', e) -> (store', If e e2 e3)) $ eval1 store e1
eval1 store (Seq Skip e2) = Just (store, e2)
eval1 store (Seq e1 e2) = fmap (\(store', e1') -> (store', Seq e1' e2))  $ eval1 store e1
eval1 store (While e1 e2) = Just (store, If e1 (Seq e2 (While e1 e2)) Skip)
eval1 store (Deref l) = fmap (\n -> (store, Integer n)) $ Store.lookup store l
eval1 store (Assign l (Integer n)) = fmap (\store' -> (store', Skip)) $ update store l n
eval1 store (Assign l e) = fmap (\(store', e') -> (store', Assign l e')) $ eval1 store e
eval1 store (Op (Integer n1) Plus (Integer n2)) = Just (store, Integer $ n1 + n2)
eval1 store (Op (Integer n1) GTEQ (Integer n2)) = Just (store, Boolean $ n1 >= n2)
eval1 store (Op e1 op e2) | isValue e1 = fmap (\(store', e2') -> (store', (Op e1 op e2'))) $ eval1 store e2
eval1 store (Op e1 op e2) = fmap (\(store', e1') -> (store', (Op e1' op e2))) $ eval1 store e1

     
eval :: Store Int -> Expression -> (Store Int, Expression)
eval store expression = case eval1 store expression of 
  Just (store', expression') -> eval store' expression' 
  Nothing -> (store, expression)

