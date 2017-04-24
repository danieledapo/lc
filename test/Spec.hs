import qualified Data.Set as Set

import Lib
import Parser

import Test.QuickCheck

instance Arbitrary AST where
  arbitrary = sized $ arbitraryAST (Set.fromList ["x", "y", "someglobals"]) -- need some globals otherwise a single Var wouldn't work

arbitraryAST :: Set.Set String -> Int -> Gen AST
arbitraryAST vars 0 = Var <$> elements (Set.toList vars)
arbitraryAST vars size = oneof [arbitraryAbs, arbitraryApp]
  where
    arbitraryAbs = do
      arg <- string
      body <- arbitraryAST (Set.insert arg vars) (size - 1)
      return $ Abs arg body
    arbitraryApp = do
      size' <- choose (0, size - 1)
      App <$> arbitraryAST vars size' <*> arbitraryAST vars (size - size')

string :: Gen String
string = listOf1 char

char :: Gen Char
char = elements ['a' .. 'z']

prop_parsePrettyPrint :: AST -> Bool
prop_parsePrettyPrint a =
  let (Right r) = parse line "<test>" (render . pPrint $ a)
  in r == a

-- verboseTest = verboseCheckWith stdArgs {maxSize = 10}

main :: IO ()
main = quickCheck prop_parsePrettyPrint
