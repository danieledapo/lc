-- | A simple lambda calculus
module Language.Lc
  ( Lc(..)
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
data Lc
  = LcVar String -- ^ variable, a.k.a reference
  | LcAbs String Lc -- ^ abstraction, a.k.a. anonymous function that takes one argument and returns one value
  | LcApp Lc Lc -- ^ function application, a.k.a call a lambda with an argument
  deriving (Show, Eq)

instance Pretty Lc where
  -- | pretty print the AST into a human readable form
  pPrint (LcVar v) = text v
  pPrint (LcAbs arg body) = char lam <> text arg <> char '.' <> pPrint body
  pPrint (LcApp fn arg) =
    let parensIfNotVar p = maybeParens (not $ isVar p) (pPrint p)
    in parensIfNotVar fn <+> parensIfNotVar arg

isVar :: Lc -> Bool
isVar (LcVar _) = True
isVar _ = False
