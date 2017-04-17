module Main where

import Parser

main :: IO ()
main = mapM_ (\l -> putStr l >> putStr " -> " >> parseTest line l) input
  where
    input = ["x", "λx.x", "x y", "(λx y.x) a b"]
