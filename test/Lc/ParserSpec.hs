module Lc.ParserSpec where

import Language.Lc
import Language.Lc.Parser

import Lc.Arbitrary ()

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty(..))


--------------------------------------------------------------
-- Parser
--------------------------------------------------------------

parseLine :: String -> Maybe Lc
parseLine = parseMaybe line

spec :: Spec
spec = parallel $
  describe "parser" $ do
    it "gives the original Lc if the latter was pretty printed" $
      property (\lc -> Just lc == parseLine (render . pPrint $ lc))

    it "correctly parses some examples" $
      mapM_
        (\(s, e) -> parseLine s `shouldBe` Just e)
        [ ("x", LcVar "x")
        , ("λx.x", LcAbs "x" (LcVar "x"))
        , ("x y", LcApp (LcVar "x") (LcVar "y"))
        , ("λx y.x", LcAbs "x" (LcAbs "y" (LcVar "x")))
        , ( "(λx y.x) a b"
          , LcApp
                 (LcApp
                       (LcAbs "x" (LcAbs "y" (LcVar "x")))
                       (LcVar "a"))
                 (LcVar "b"))
        ]
