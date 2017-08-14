module Lc.Arbitrary where


import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Lc

import Test.QuickCheck


--------------------------------------------------------------
-- QuickCheck - Arbitratry
--------------------------------------------------------------

-- use some globals otherwise a single Var wouldn't work
globals :: Set.Set String
globals = Set.fromList ["x", "y", "someglobals"]

initialVars :: Map.Map String Integer
initialVars = Map.fromList $ zip (Set.toList globals) [0 ..]

instance Arbitrary Lc where
  arbitrary = sized $ arbitraryLc globals

arbitraryLc :: Set.Set String -> Int -> Gen Lc
arbitraryLc vars 0    = LcVar <$> elements (Set.toList vars)
arbitraryLc vars size = oneof [arbitraryAbs, arbitraryApp]
 where
  arbitraryAbs = do
    arg  <- string
    body <- arbitraryLc (Set.insert arg vars) (size - 1)
    return $ LcAbs arg body

  arbitraryApp = do
    size' <- choose (0, size - 1)
    lhs   <- arbitraryLc vars size'
    rhs   <- arbitraryLc vars (size - 1 - size')
    return $ LcApp lhs rhs


string :: Gen String
string = listOf1 char

char :: Gen Char
char = elements ['a' .. 'z']


