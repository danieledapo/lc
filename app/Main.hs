module Main where

import Lib
import Parser

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
