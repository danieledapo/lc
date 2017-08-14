module ELc.ExecSpec where

import Control.Monad.State

import Language.ELc
import Language.Lc

import ELc.Arbitrary ()

import Test.Hspec
import Test.QuickCheck


--------------------------------------------------------------
-- Exec
--------------------------------------------------------------

run :: ELc -> Lc
run elc = head . runAll $ [elc]

runAll :: [ELc] -> [Lc]
runAll allElcs = evalState (go allElcs) emptyExecEnv
 where
  go :: [ELc] -> ExecState [Lc]
  go []         = return []
  go (elc:elcs) = do
    val  <- naiveInterpreter `exec` elc
    vals <- go elcs
    return $ val : vals

runWithLets :: [Let] -> ELc -> Lc
runWithLets lets elc = evalState (naiveInterpreter `exec` elc) (ExecEnv lets)

spec :: Spec
spec = describe "exec" $ do
  it "always evaluates the value of a let" $ property prop_LetEvaluesToValue

  it "correctly evaluates some examples" $ mapM_
    (\(elcs, ex) -> last (runAll elcs) `shouldBe` ex)
    [ ([ELc (LcVar "y")], LcVar "y")
    , ([ELcLet (Let "y" (LcVar "x")), ELc (LcVar "y")], LcVar "x")
    , ( [ ELcLet (Let "id" (LcAbs "x" (LcVar "x")))
        , ELcLet (Let "foo" (LcApp (LcVar "id") (LcVar "a")))
        , ELc (LcVar "foo")
        ]
      , LcVar "a"
      )
    ]


prop_LetEvaluesToValue :: Let -> Bool
prop_LetEvaluesToValue let_@(Let _ value) = eval naiveInterpreter value == run (ELcLet let_)
