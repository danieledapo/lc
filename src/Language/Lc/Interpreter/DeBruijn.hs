{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

-- | Beta reduce using DeBruijn indexes
module Language.Lc.Interpreter.DeBruijn
  ( DeBruijn(..)
  , DeBruijnVar(..)

  -- interpreter
  , deBruijnInterpreter

  -- utility
  , boundVars
  , foldVars
  , freeVars
  , mapBoundVars
  , mapFreeVars
  , mapVars
  , vars
  ) where

import Language.Lc

import Data.List (genericIndex)
import qualified Data.Map as Map

-- | a 'DeBruijn' variable
data DeBruijnVar
  = Free String -- ^ a free 'Var', not bound by any 'Abs'
                -- we keep the original name to call 'toLc'
  | Index Integer -- ^ the index of the 'Abs' the 'Var' was passed to
  deriving (Eq, Show)

-- | use De 'Bruijn's indexes to evaluate the 'Lc'.
-- This is a strict interpreter because if a variable is not in scope when
-- defining the expression it will be free.
-- For example, `(λfoo. (foo x) b) (λx y. y (x x))` eventually betaReduces to `x (b b)` since
-- the `x` inside `(foo x) b` is free when declaring that expression
--
-- here's an example of a 'Lc' that will give different results
--
-- @
--     LcApp
--      ( LcAbs
--        "foo"
--        ( LcApp
--          (LcApp (LcVar "foo") (LcVar "x"))
--          ( LcApp (LcAbs "bz" (LcAbs "kaawc" (LcAbs "bar" (LcAbs "gu" (LcVar "someglobals")))))
--                  (LcVar "y")
--          )
--        )
--      )
--      ( LcAbs "baz"
--              (LcAbs "x" (LcApp (LcVar "baz") (LcApp (LcVar "x") (LcVar "x"))))
--      )
-- @
--

data DeBruijn
  = DVar DeBruijnVar -- ^ a 'DeBruijn' doc
  | DAbs String
         DeBruijn -- ^ an 'Abs', note that we keep the original name only to call 'toLc'
  | DApp DeBruijn
         DeBruijn -- ^ an 'App'
  deriving (Eq, Show)


--------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------

-- | use 'DeBruijn' indexes to interpret a given 'Lc'
deBruijnInterpreter :: DeBruijnInterpreter
deBruijnInterpreter = DeBruijnInterpreter

data DeBruijnInterpreter = DeBruijnInterpreter

instance Interpreter DeBruijnInterpreter where
  type InternalLc DeBruijnInterpreter = DeBruijn
  fromLc _ = deBruijnFromLc
  toLc _ = deBruijnToLc
  betaReduceI _ = deBruijnBetaReduce


-- | 'betaReduce' the given 'DeBruijn', if it's not a 'DApp'
-- then return the original 'DeBruijn'.
-- Commonly called function application
deBruijnBetaReduce :: DeBruijn -> DeBruijn
deBruijnBetaReduce (DApp (DAbs _ fn) arg) = substitute fn 0 arg
deBruijnBetaReduce (DApp fn arg) =
  let fn'  = deBruijnBetaReduce fn
      arg' = deBruijnBetaReduce arg
  in  DApp fn' arg' -- ensure both fn and arg have already been reduced
deBruijnBetaReduce x = x


-- | substitute in the given 'DeBruijn' the Var of the given 'Index'
-- with the given 'DeBruijn'. It respects all the substitution laws.
substitute
  :: DeBruijn -- ^ the 'DeBruijn' to search into
  -> Integer -- ^ the 'Var's index that we want to replace with
  -> DeBruijn -- ^ the 'DeBruijn' to use as the replacement
  -> DeBruijn
substitute v@(DVar (Index ix)) i x = if ix == i then x else v
substitute v@(DVar (Free  _ )) _ _ = v
substitute (  DAbs a  body   ) i x = DAbs a $ substitute body (i + 1) x
substitute (  DApp fn arg    ) i x = DApp (substitute fn i x) (substitute arg i x)


--------------------------------------------------------------
-- Conversion back and forth between Lc and DeBruijn
--------------------------------------------------------------

-- | convert back from 'DeBruijn' to 'Lc'
deBruijnToLc :: DeBruijn -> Lc
deBruijnToLc = go []
 where
  go _        (DVar (Free  v )) = LcVar v
  go varStack (DVar (Index ix)) = LcVar $ varStack `genericIndex` ix
  go varStack (DApp fn    arg ) = LcApp (go varStack fn) (go varStack arg)
  go varStack (DAbs param body) = LcAbs param (go (param : varStack) body)

-- | state to pass around when converting 'Lc' to 'DeBruijn'
data FromLcState = FromLcState
  { depth :: Integer -- ^ a.k.a number of Abs met so far
  , varToAbsIx :: Map.Map String Integer -- ^ var to the lambda index it was defined in
  } deriving (Eq, Show)

-- | convert a 'Lc' to the 'DeBruijn' representation
deBruijnFromLc :: Lc -> DeBruijn
deBruijnFromLc = fromLcWithState FromLcState
  { depth      = 0
  , varToAbsIx = Map.empty
  }

fromLcWithState :: FromLcState -> Lc -> DeBruijn
fromLcWithState state (LcVar v) = case Map.lookup v (varToAbsIx state) of
  Just varDepth -> DVar . Index $ depth state - varDepth
  Nothing       -> DVar . Free $ v

fromLcWithState state (LcApp fn arg) = DApp fn' arg'
 where
  fn'  = fromLcWithState state fn
  arg' = fromLcWithState state arg

fromLcWithState state (LcAbs x body) = DAbs x (fromLcWithState state' body)
 where
  state'   = state { depth = incDepth, varToAbsIx = Map.insert x incDepth (varToAbsIx state) }
  incDepth = depth state + 1


--------------------------------------------------------------
-- Utility
--------------------------------------------------------------

-- | fold 'DVar' using f with initial accumulator a
foldVars :: (a -> DeBruijnVar -> a) -> a -> DeBruijn -> a
foldVars f acc (DVar dbi    ) = f acc dbi
foldVars f acc (DAbs _  body) = foldVars f acc body
foldVars f acc (DApp fn arg ) = let acc' = foldVars f acc fn in foldVars f acc' arg

-- | map the given f to all the 'DVar' in db
mapVars :: (DeBruijnVar -> a) -> DeBruijn -> [a]
mapVars f = reverse . foldVars (\acc v -> f v : acc) []

-- | return all the 'DVar' in db
vars :: DeBruijn -> [DeBruijnVar]
vars = mapVars id

-- | map the given f to all the free 'DVar' in db
mapFreeVars :: (String -> a) -> DeBruijn -> [a]
mapFreeVars f = reverse . foldVars go []
 where
  go acc (Free v) = f v : acc
  go acc _        = acc

-- | return all the free 'DVar' in db
freeVars :: DeBruijn -> [String]
freeVars = mapFreeVars id

-- | map the given f to all the bound 'DVar' in db
mapBoundVars :: (Integer -> a) -> DeBruijn -> [a]
mapBoundVars f = reverse . foldVars go []
 where
  go acc (Index ix) = f ix : acc
  go acc _          = acc

-- | return all the bound 'DVar' in db
boundVars :: DeBruijn -> [Integer]
boundVars = mapBoundVars id
