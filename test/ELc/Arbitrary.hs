module ELc.Arbitrary where

import Language.ELc

import Lc.Arbitrary

import Test.QuickCheck


--------------------------------------------------------------
-- QuickCheck - Arbitratry
--------------------------------------------------------------

instance Arbitrary ELc where
  arbitrary = arbitraryELc

instance Arbitrary Let where
  arbitrary = arbitraryLet

arbitraryELc :: Gen ELc
arbitraryELc = oneof [ELcLet <$> arbitraryLet, ELc <$> arbitrary]

arbitraryLet :: Gen Let
arbitraryLet = Let <$> string <*> arbitrary
