module Lib
  ( someFunc
  ) where

lam :: Char
lam = '\955' -- aka \03BB

someFunc :: IO ()
someFunc = putStrLn [lam]
