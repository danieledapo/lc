module Lc.NaiveSpec where

import Language.Lc

import Lc.Interpreter

import Test.Hspec


spec :: Spec
spec = parallel $ describe "naiveInterpreter" $ interpreterSpec naiveInterpreter
