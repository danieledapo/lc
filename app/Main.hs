module Main where

import Control.Monad.State

import Language.ELc
import qualified Language.ELc.Parser as EP
import Language.Lc
import Language.Lc.DeBruijn
import qualified Language.Lc.Parser as P

import System.Console.Haskeline

import Text.Megaparsec
import Text.Megaparsec.String

import Text.PrettyPrint.HughesPJClass (Pretty(..))


--------------------------------------------------------------
-- Cli
--------------------------------------------------------------

type Cli a = ExecStateT (InputT IO) a

runCli :: Cli a -> ExecEnv -> IO a
runCli cli env = runInputT defaultSettings (evalStateT cli env)

output :: String -> Cli ()
output = lift . outputStrLn


--------------------------------------------------------------
-- Main
--------------------------------------------------------------

main :: IO ()
main = runCli interactive initialExecEnv


--------------------------------------------------------------
-- Interactive
--------------------------------------------------------------

interactive :: Cli ()
interactive = do
  mline <- lift $ getInputLine prompt
  case mline of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just line   -> do
      processLine line
      interactive
  where prompt = lam : " >> "

processLine :: String -> Cli ()
processLine inp = case parse lineOrNothing "<stdin>" inp of
  Left  err        -> printErr err
  Right Nothing    -> return ()
  Right (Just elc) -> do
    execEnv <- get
    let (lc, execEnv') = runState (deBruijnInterpreter `exec` elc) execEnv

    put execEnv'
    output . show . pPrint $ lc
  where printErr = output . parseErrorPretty


--------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------

lineOrNothing :: Parser (Maybe ELc)
lineOrNothing = try line <|> nothing
 where
  line    = fmap Just (EP.expr <* P.space)
  nothing = P.space >> eof >> return Nothing


--------------------------------------------------------------
-- Stdlib
--------------------------------------------------------------

initialExecEnv :: ExecEnv
initialExecEnv =
  let st = foldM_ (const f) () stdlib in execState st emptyExecEnv
  where f let_ = void $ deBruijnInterpreter `exec` ELcLet let_


stdlib :: [Let]
stdlib = fmap
  conv
  [ -- S.K.I.
    "id = λx.x"
  , "const = λx y.x"
  , "s = λx y z.x z (y z)"

    -- omega and Y
  , "omega = (λx.x x) (λx.x x)"
  , "Y = λf. (λx. f (x x)) (λx. f (x x))"

    -- Church booleans
  , "true = λx y.x" -- a.k.a const
  , "false = λx y.y"
  , "if = λp t e. p t e"
  , "and = λp q. p q p"
  , "or = λp q. p p q"
  , "not = λp x y. p y x"

    -- Church numbers
  , "0 = λf x. x"
  , "succ = λn f x. f (n f x)"
  , "pred = λn f x. n (λg h. h (g f)) (λu.x) (λu.u)" -- this is hard, really
  , "add = λm n. n succ m"
  , "sub = λm n. n pred m"
  , "mul = λm n f. m (n f)"
  , "is0 = λn. n (λ_. false) true"
  , "nLeq = λn m. is0 (sub n m)"
  , "nEq = λn m. and (nLeq n m) (nLeq m n)"
  , "nLe = λn m. and (nLeq n m) (not (nLeq m n))"
  , "nGeq = λn m. nLeq m n"
  , "nGe = λn m. and (nGeq n m) (not (nEq n m))"
  ]
  where conv l = let (Just lc) = parseMaybe EP.let_ l in lc
