module AST where

import Store

-- Define AST


data Operator = Plus | GTEQ
  deriving (Eq, Show)

data Expression = Integer Int
                | Boolean Bool
                | Skip 
                | If Expression Expression Expression
                | Seq Expression Expression
                | While Expression Expression
                | Op Expression Operator Expression
                | Assign Location Expression
                | Deref Location
  deriving (Eq, Show)
