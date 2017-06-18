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
main = runCli interactive emptyExecEnv


--------------------------------------------------------------
-- Interactive
--------------------------------------------------------------

interactive :: Cli ()
interactive = do
  mline <- lift $ getInputLine prompt
  case mline of
    Nothing -> return ()
    Just "quit" -> return ()
    Just line -> do
      processLine line
      interactive
  where
    prompt = lam : " >> "

processLine :: String -> Cli ()
processLine inp =
  case parse lineOrNothing "<stdin>" inp of
    Left err -> printErr err
    Right Nothing -> return ()
    Right (Just elc) -> do
      execEnv <- get
      let (lc, execEnv') = runState (deBruijnInterpreter `exec` elc) execEnv

      put execEnv'
      output . show . pPrint $ lc
  where
    printErr = output . parseErrorPretty


--------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------

lineOrNothing :: Parser (Maybe ELc)
lineOrNothing = try line <|> nothing
  where
    line = fmap Just (EP.expr <* P.space)
    nothing = P.space >> eof >> return Nothing
