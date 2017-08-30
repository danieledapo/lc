{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Language.Lc.Interpreter.Dynamic
  ( dynamicInterpreter
  ) where


import Language.Lc

--------------------------------------------------------------
-- Dynamic interpreter
--------------------------------------------------------------
-- | dynamic interpreter, it uses 'dynamicBetaReduce'
-- This is a dynamic interpreter because the variable is retrieved only when it's needed even
-- though it might not be in scope when defining the expression. It is not a totally valid
-- interpreter but it is interesting to have.
-- For example, `(λfoo. (foo x) b) (λx y. y (x x))` eventually betaReduces to `b (b b)` since
-- the `x` inside `(foo x) b` eventually resolves to `b`
dynamicInterpreter :: DynamicInterpreter
dynamicInterpreter = DynamicInterpreter

data DynamicInterpreter =
  DynamicInterpreter

instance Interpreter DynamicInterpreter where
  type InternalLc DynamicInterpreter = Lc
  fromLc _ = id
  toLc _ = id
  betaReduceI _ = dynamicBetaReduce

-- | dynamic 'betaReduce'
dynamicBetaReduce :: Lc -> Lc
dynamicBetaReduce (LcApp (LcAbs p fn) arg) = substitute fn p arg
dynamicBetaReduce mainApp@(LcApp fn arg) =
  let fn' = dynamicBetaReduce fn
      arg' = dynamicBetaReduce arg
  in if fn /= fn' || arg /= arg' -- ensure both fn and arg have already been reduced
       then LcApp fn' arg'
       else mainApp
dynamicBetaReduce lc = lc

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
