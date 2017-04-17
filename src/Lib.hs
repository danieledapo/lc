module Lib
  ( AST(..)
  , lam
  ) where

lam :: Char
lam = '\955' -- aka 0x03BB

data AST
  = Var String
  | Abs String
        AST
  | App AST
        AST
  deriving (Show, Eq)
