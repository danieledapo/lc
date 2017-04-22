-- | A simple lambda calculus
module Lib
  ( AST(..)
  , lam
  , pPrint
  , render
  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty(..))

-- | Unicode lambda, a.k.a 0x03BB
lam :: Char
lam = '\955'

-- | the AST of the vanilla untyped lambda calculus
data AST
  = Var String -- ^ variable, a.k.a reference
  | Abs String
        AST -- ^ abstraction, a.k.a. anonymous function that takes one argument
            -- ^ and return one value
  | App AST
        AST -- ^ function application, a.k.a call a lambda with an argument
  deriving (Show, Eq)

instance Pretty AST where
  -- | pretty print the AST into a human readable form
  pPrint (Var v) = text v
  pPrint (Abs arg body) = char lam <> text arg <> char '.' <> pPrint body
  pPrint (App fn arg) =
    let parensIfNotVar p = maybeParens (not $ isVar p) (pPrint p)
    in parensIfNotVar fn <+> parensIfNotVar arg

isVar :: AST -> Bool
isVar (Var _) = True
isVar _ = False
