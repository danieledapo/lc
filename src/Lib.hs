module Lib
  ( AST(..)
  , lam
  , pPrint
  , render
  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty(..))

lam :: Char
lam = '\955' -- aka 0x03BB

data AST
  = Var String
  | Abs String
        AST
  | App AST
        AST
  deriving (Show, Eq)

instance Pretty AST where
  pPrint (Var v) = text v
  pPrint (Abs arg body) = char lam <> text arg <> char '.' <> pPrint body
  pPrint (App fn arg) =
    let parensIfNotVar p = maybeParens (not $ isVar p) (pPrint p)
    in parensIfNotVar fn <+> parensIfNotVar arg

isVar :: AST -> Bool
isVar (Var _) = True
isVar _ = False
