module Main where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty(..))

import Language.Lc.Parser

main :: IO ()
main = mapM_ go input
  where
    go l = do
      putStr l
      putStr " -> "
      let (Right a) = parse line "<test>" l
      putStr . show $ a
      putStr " -> "
      putStrLn . render . pPrint $ a
    input = ["x", "λx.x", "x y", "λx y.x", "(λx y.x) a b"]
