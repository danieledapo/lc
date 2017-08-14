module Lc.Interpreter where

import Language.Lc

import Test.Hspec


interpreterSpec :: Interpreter i => i -> SpecWith ()
interpreterSpec interpreter = context "betaReduce" $ do
  it "correctly betaReduce I" $ betaReduceAll' (appLcId (LcVar "a")) `shouldBe` [LcVar "a"]

  it "correctly betaReduce K" $ do
    let got = betaReduceAll' $ appLcK (LcVar "a") (LcVar "b")
    got `shouldBe` [LcApp (LcAbs "y" (LcVar "a")) (LcVar "b"), LcVar "a"]

  it "correctly betaReduce S" $ do
    let got = betaReduceAll' $ appLcS (LcVar "a") (LcVar "b") (LcVar "c")
    let
      expected
        = [ LcApp
            ( LcApp
              ( LcAbs
                "y"
                (LcAbs "z" (LcApp (LcApp (LcVar "a") (LcVar "z")) (LcApp (LcVar "y") (LcVar "z"))))
              )
              (LcVar "b")
            )
            (LcVar "c")
          , LcApp
            (LcAbs "z" (LcApp (LcApp (LcVar "a") (LcVar "z")) (LcApp (LcVar "b") (LcVar "z"))))
            (LcVar "c")
          , LcApp (LcApp (LcVar "a") (LcVar "c")) (LcApp (LcVar "b") (LcVar "c"))
          ]
    got `shouldBe` expected

  it "correctly eval that SKK is I" $ eval' (appLcS lcK lcK (LcVar "ski")) `shouldBe` LcVar "ski"

  it "correctly betaReduce omega" $ do
    betaReduce' lcOmega `shouldBe` lcOmega
    eval' lcOmega `shouldBe` lcOmega

  it "correctly betaReduce succ 0 f x" $ do
    let expected = LcApp (LcVar "f") (LcVar "x")
    eval' succ0 `shouldBe` expected
 where
  betaReduce'    = betaReduce interpreter
  betaReduceAll' = betaReduceAll interpreter
  eval'          = eval interpreter


--------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------

-- I combinator: \x.x
lcId :: Lc
lcId = LcAbs "x" (LcVar "x")

appLcId :: Lc -> Lc
appLcId = LcApp lcId

-- K combinator(aka const): \x y.x
lcK :: Lc
lcK = LcAbs "x" (LcAbs "y" (LcVar "x"))

appLcK :: Lc -> Lc -> Lc
appLcK x = LcApp (LcApp lcK x)

-- S combinator: \x y z. x z (y z)
lcS :: Lc
lcS =
  let xz   = LcApp (LcVar "x") (LcVar "z")
      yz   = LcApp (LcVar "y") (LcVar "z")
      body = LcApp xz yz
  in  LcAbs "x" (LcAbs "y" (LcAbs "z" body))

appLcS :: Lc -> Lc -> Lc -> Lc
appLcS x y = LcApp (LcApp (LcApp lcS x) y)


-- omega: (\w. w w) (\w. w w)
lcOmega :: Lc
lcOmega = LcApp lcW lcW where lcW = LcAbs "w" $ LcApp (LcVar "w") (LcVar "w")


-- succ 0 fn x
succ0 :: Lc
succ0 = LcApp
  ( LcAbs
    "0"
    ( LcApp
      ( LcAbs
        "succ"
        ( LcApp (LcVar "f")
                (LcApp (LcApp (LcAbs "f" (LcAbs "x" (LcVar "x"))) (LcVar "f")) (LcVar "x"))
        )
      )
      ( LcAbs
        "n"
        ( LcAbs
          "f"
          (LcAbs "x" (LcApp (LcVar "f") (LcApp (LcApp (LcVar "n") (LcVar "f")) (LcVar "x"))))
        )
      )
    )
  )
  (LcAbs "f" (LcAbs "x" (LcVar "x")))
