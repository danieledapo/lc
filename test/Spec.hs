import qualified Data.Set as Set

import Language.Lc
import Language.Lc.Parser

import Test.QuickCheck

instance Arbitrary Lc where
  arbitrary = sized $ arbitraryLc (Set.fromList ["x", "y", "someglobals"]) -- need some globals otherwise a single Var wouldn't work

arbitraryLc :: Set.Set String -> Int -> Gen Lc
arbitraryLc vars 0 = LcVar . Var <$> elements (Set.toList vars)
arbitraryLc vars size = oneof [arbitraryAbs, arbitraryApp]
  where
    arbitraryAbs = do
      arg <- string
      body <- arbitraryLc (Set.insert arg vars) (size - 1)
      return . LcAbs $ Abs arg body
    arbitraryApp = do
      size' <- choose (0, size - 1)
      lhs <- arbitraryLc vars size'
      rhs <- arbitraryLc vars (size - size')
      return . LcApp $ App lhs rhs

string :: Gen String
string = listOf1 char

char :: Gen Char
char = elements ['a' .. 'z']

prop_parsePrettyPrint :: Lc -> Bool
prop_parsePrettyPrint a =
  let (Right r) = parse line "<test>" (render . pPrint $ a)
  in r == a

-- verboseTest = verboseCheckWith stdArgs {maxSize = 10}

main :: IO ()
main = quickCheck prop_parsePrettyPrint
