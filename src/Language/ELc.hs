-- | enhanced Lc
module Language.ELc
  ( Let(..)
  , ELc(..)
  , emptyExecEnv

  -- exec
  , ExecEnv(..)
  , ExecState
  , ExecStateT
  , exec
  , expandLet
  , expandLets
  ) where

import Control.Monad.State

import Language.Lc

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty(..))


--------------------------------------------------------------
-- Core
--------------------------------------------------------------

-- | a 'Let' binding
data Let =
  Let String
      Lc -- ^ the name of the binding and its value
  deriving (Eq, Show)

-- | an enhanced 'Lc'
data ELc
  = ELcLet Let -- ^ that supports 'Let' bindings
  | ELc Lc -- ^ and the plain 'Lc'
  deriving (Eq, Show)

instance Pretty ELc where
  pPrint (ELcLet (Let name lc)) = text name <+> char '=' <+> pPrint lc
  pPrint (ELc lc) = pPrint lc

-- | the environment 'exec' runs in
newtype ExecEnv = ExecEnv
  { _lets :: [Let] -- ^ in reverse order, most recent let first
  } deriving (Eq, Show)

-- | new empty 'ExecEnv'
emptyExecEnv :: ExecEnv
emptyExecEnv = ExecEnv []


--------------------------------------------------------------
-- Exec
--------------------------------------------------------------

-- | just 'State' 'ExecEnv'
type ExecState = State ExecEnv

-- | just 'StateT' 'ExecEnv'
type ExecStateT = StateT ExecEnv

-- | exec the given 'Interpreter' on the given 'ELc' returning
-- the final 'Lc' and the new state. In case of an 'ELcLet' the
-- value is evaluated
exec
  :: Interpreter i
  => i -> ELc -> ExecState Lc
exec interpreter (ELc lc) = do
  lets <- gets _lets
  let lc' = expandLets lc lets
  return $ interpreter `eval` lc'

exec interpreter (ELcLet (Let name value)) = do
  lc <- exec interpreter (ELc value)
  let newLet = Let name lc
  modify (\(ExecEnv lets) -> ExecEnv (newLet : lets))
  return lc  -- exec the value just because yeah


-- | tiny utility to expand all the given 'Let's, see 'expandLet'
expandLets :: Lc -> [Let] -> Lc
expandLets = foldl expandLet

-- | 'Let' is nothing, but syntactic sugar for injecting
-- a value under a name. Consider `let a = b in c`, that can
-- be written as `((\a -> c) b)`
expandLet :: Lc -> Let -> Lc
expandLet lc (Let name value) = LcApp (LcAbs name lc) value
