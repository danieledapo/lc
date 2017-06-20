{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

-- | A simple lambda calculus
module Language.Lc
  (
  -- core
    Lc(..)
  , lam

  -- interpreter
  , Interpreter(..)
  , betaReduce
  , betaReduceAll
  , eval
  , naiveInterpreter

  ) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty(..))


--------------------------------------------------------------
-- Core
--------------------------------------------------------------

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
    where
      isVar :: Lc -> Bool
      isVar (LcVar _) = True
      isVar _ = False


--------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------

-- | Interpreter, 'betaReduce' the 'Lc'
class Eq (InternalLc i) => Interpreter i where
  type InternalLc i :: *
  fromLc :: i -> Lc -> InternalLc i
  toLc :: i -> InternalLc i -> Lc
  betaReduceI :: i -> InternalLc i  -> InternalLc i


-- | evaluate the given 'Lc', returns the given one if it's
-- not reducible
eval :: Interpreter i => i -> Lc -> Lc
eval i lc = case betaReduceAll i lc of
  [] -> lc
  lcs -> last lcs


-- | 'betaReduce' the given 'Lc' until no further reductions
-- are possible
betaReduceAll :: Interpreter i => i -> Lc -> [Lc]
betaReduceAll interpreter = go . fromLc interpreter
  where
    go ilc =
      let ilc' = interpreter `betaReduceI` ilc
      in if ilc /= ilc'
          then interpreter `toLc` ilc' : go ilc'
          else []


-- | betaReduce the given 'Lc', if it's not a 'LcApp'
-- then return the original 'Lc'.
-- Commonly called function application
betaReduce :: Interpreter i => i -> Lc -> Lc
betaReduce interpreter =
  toLc interpreter . betaReduceI interpreter . fromLc interpreter


--------------------------------------------------------------
-- Naive interpreter
--------------------------------------------------------------

-- | naive interpreter, it uses 'naiveBetaReduce'
naiveInterpreter :: NaiveInterpreter
naiveInterpreter = NaiveInterpreter

data NaiveInterpreter = NaiveInterpreter

instance Interpreter NaiveInterpreter where
  type InternalLc NaiveInterpreter = Lc
  fromLc _ = id
  toLc _ = id
  betaReduceI _ = naiveBetaReduce


-- | naive 'betaReduce'
naiveBetaReduce :: Lc -> Lc
naiveBetaReduce (LcApp (LcAbs p fn) arg) = substitute fn p arg
naiveBetaReduce mainApp@(LcApp fn arg) =
  let fn' = naiveBetaReduce fn
      arg' = naiveBetaReduce arg
  in if fn /= fn' || arg /= arg'  -- ensure both fn and arg have already been reduced
       then LcApp fn' arg'
       else mainApp
naiveBetaReduce lc = lc

-- | substitute in the given 'Lc' the LcVars with the given name
-- with the given 'Lc'
substitute :: Lc -> String -> Lc -> Lc
substitute v@(LcVar var) param new =
  if var == param
    then new
    else v

substitute lcAbs@(LcAbs newParam fn) oldParam new =
  if newParam /= oldParam
    then LcAbs newParam $ substitute fn oldParam new
    else lcAbs -- oldParam has been shadowed in this case

substitute (LcApp fn arg) param new =
  LcApp (substitute fn param new) (substitute arg param new)

