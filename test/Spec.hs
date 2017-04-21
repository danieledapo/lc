import Lib
import Parser

import Test.QuickCheck

instance Arbitrary AST where
  arbitrary = do
    n <- elements [0..2]
    case n of
      0 -> fmap Var string
      1 -> Abs <$> string <*> arbitrary
      2 -> App <$> arbitrary <*> arbitrary

string :: Gen String
string = listOf1 char

char :: Gen Char
char = elements ['a' .. 'z']

prop_parsePrettyPrint :: AST -> Bool
prop_parsePrettyPrint a =
  let (Right r) = parse line "<test>" (render . pPrint $ a)
  in r == a

main :: IO ()
main = quickCheck prop_parsePrettyPrint
