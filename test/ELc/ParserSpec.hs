module ELc.ParserSpec where

import Language.ELc
import Language.ELc.Parser
import Language.Lc

import ELc.Arbitrary ()

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty(..))


--------------------------------------------------------------
-- Parser
--------------------------------------------------------------

parseLine :: String -> Maybe ELc
parseLine = parseMaybe expr


spec :: Spec
spec =
  describe "parser" $ do
    it "gives the original ELc if the latter was pretty printed" $
      property (\elc -> Just elc == parseLine (render . pPrint $ elc))

    it "correctly parses some examples" $
      mapM_
        (\(s, e) -> parseLine s `shouldBe` Just e)
        [ ("x", ELc (LcVar "x"))
        , ("λx.x", ELc (LcAbs "x" (LcVar "x")))
        , ("x y", ELc (LcApp (LcVar "x") (LcVar "y")))
        , ("x = b", ELcLet (Let "x" (LcVar "b")))
        , ("x = λx.x", ELcLet (Let "x" (LcAbs "x" (LcVar "x"))))
        , ("x = a b", ELcLet (Let "x" (LcApp (LcVar "a") (LcVar "b"))))
        ]
