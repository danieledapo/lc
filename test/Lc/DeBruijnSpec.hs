module Lc.DeBruijnSpec where

import qualified Data.Set as Set

import Language.Lc
import Language.Lc.DeBruijn

import Lc.Arbitrary
import Lc.Interpreter

import Safe

import Test.Hspec
import Test.QuickCheck


--------------------------------------------------------------
-- DeBruijn
--------------------------------------------------------------

dVar :: Integer -> DeBruijn
dVar = DVar . Index

dFree :: String -> DeBruijn
dFree = DVar . Free

toLc' :: DeBruijn -> Lc
toLc' = toLc deBruijnInterpreter

fromLc' :: Lc -> DeBruijn
fromLc' = fromLc deBruijnInterpreter


spec :: Spec
spec = parallel $
  describe "DeBruijn" $ do
    context "conversion back and forth between Lc and DeBruijn" $ do
      it "gives the original Lc" $
        property (\lc -> (toLc' . fromLc' $ lc) == lc)

      it "generates indexes between 0 and the number of Abs" $
        property prop_deBruijnIndexes

      it "converts from and to some examples Lc" $
        mapM_
          (\(lc, e) ->
            let db = fromLc' lc
            in (db, toLc' db) `shouldBe` (e, lc))
          [ (LcVar "x", dFree "x")
          , (LcAbs "x" (LcVar "x"), DAbs "x" (dVar 0))
          , (LcApp (LcVar "x") (LcVar "y"), DApp (dFree "x") (dFree "y"))
          , (LcAbs "x" (LcAbs "y" (LcVar "x")), DAbs "x" (DAbs "y" (dVar 1)))
          ]

    interpreterSpec deBruijnInterpreter

    context "traversal" $
      it "returns all the vars" $ do
        vars exampleDb `shouldBe` [Free "x", Index 0, Index 1]
        freeVars exampleDb `shouldBe` ["x"]
        boundVars exampleDb `shouldBe` [0, 1]

  where
    exampleDb = DApp (dFree "x") (DAbs "y" (DApp (dVar 0) (DAbs "z" (dVar 1))))


-- min >= 0 && max <= number of Abs &&
-- the set of free vars must contain only the defined globals
prop_deBruijnIndexes :: Lc -> Gen Bool
prop_deBruijnIndexes lc = sized go
  where
    go :: Int -> Gen Bool
    go size =
      let db = fromLc' lc
          allVars = vars db
          bvars = boundVars db
          ma = maximumDef 0 bvars
          mi = minimumDef 0 bvars
          frees = Set.fromList . freeVars $ db
      in return $ length allVars <= size + 1 &&
         mi >= 0 && mi <= ma && ma <= fromIntegral size &&
         Set.null (frees `Set.difference` globals)

