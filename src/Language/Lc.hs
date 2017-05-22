-- | A simple lambda calculus
module Language.Lc
  ( Lc(..)
  , naiveBetaReduce
  , naiveBetaReduceAll
  , naiveEval
  , lam
  , pPrint
  , render
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

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


--------------------------------------------------------------
-- Naive interpreter
--------------------------------------------------------------

-- | the environment a LcAbs is evaluated in
type Environment = Map.Map String Lc

-- | evaluate the given Lc, returns the given one if it's
-- not reducible
naiveEval :: Lc -> Lc
naiveEval lc = case naiveBetaReduceAll lc of
  [] -> lc
  bs -> last bs

-- | betaReduce the given Lc until no further reductions
-- are possible
naiveBetaReduceAll :: Lc -> [Lc]
naiveBetaReduceAll lc =
  let lc' = naiveBetaReduce lc
  in if lc /= lc'
      then lc' : naiveBetaReduceAll lc'
      else []

-- | naive betaReduce
naiveBetaReduce :: Lc -> Lc
naiveBetaReduce = betaReduceWithEnv Map.empty


-- | betaReduce in the given environment
betaReduceWithEnv :: Environment -> Lc -> Lc
betaReduceWithEnv env lcV@(LcVar v) =
  fromMaybe lcV $ Map.lookup v env

betaReduceWithEnv env' (LcAbs p body) =
  let env'' = Map.delete p env'
  in LcAbs p $ betaReduceWithEnv env'' body

betaReduceWithEnv env' (LcApp (LcAbs p body) arg) =
  let env'' = Map.insert p arg env'
  in betaReduceWithEnv env'' body

betaReduceWithEnv env' app@(LcApp fn arg) =
  let fn' = betaReduceWithEnv env' fn
      arg' = betaReduceWithEnv env' arg
  in if fn /= fn' || arg /= arg'
      then LcApp fn' arg'
      else app
