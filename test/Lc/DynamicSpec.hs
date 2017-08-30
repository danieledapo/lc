module Lc.DynamicSpec where

import Language.Lc.Interpreter.Dynamic

import Lc.Interpreter

import Test.Hspec


spec :: Spec
spec = parallel $ describe "dynamicInterpreter" $ interpreterSpec dynamicInterpreter
