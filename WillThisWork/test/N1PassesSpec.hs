module N1PassesSpec (spec) where

import Test.Hspec
import Data.Int
import CompilerPasses
import N1Passes
import N1

uniquifyResult = getResult . uniquify
rcoResult prog = getResult (passRemoveComplexOperas (CState 0 (Right prog)))

spec :: Spec
spec = do
  describe "Uniqueify Tests:" $ do
    it "can uniqueify a simple let expression" $ do
      uniquifyResult (N1 (Let "x" (Int 5) (Var "x"))) `shouldBe`
        Right (N1 (Let "s0" (Int 5) (Var "s0")))

    it "can uniqueify a simple nested let expression" $ do
      uniquifyResult (N1 (Let "x" (Int 5) (Let "y" (Int 5) (Var "x"))))  `shouldBe`
        Right (N1 (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s0"))))

    it "can uniqueify a simple nested let expression" $ do
      uniquifyResult (N1 (Let "x" (Int 5) (Let "y" (Int 5) (Var "y"))))  `shouldBe`
        Right (N1 (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s1"))))

    it "can uniqueify a shadowed name nested let expression" $ do
      uniquifyResult (N1 (Let "x" (Int 5) (Let "x" (Int 5) (Var "x"))))  `shouldBe`
        Right (N1 (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s1"))))

    it "can uniqueify a nested let expression under Add" $ do
      uniquifyResult (N1 (Let "x" (Int 5) (Add (Int 6) (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Right (N1 (Let "s0" (Int 5) (Add (Int 6) (Let "s1" (Int 5) (Var "s1")))))

    it "can uniqueify a nested let expression after recurssion" $ do
      uniquifyResult (N1 (Let "x" (Int 5) (Add (Var "x") (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Right (N1 (Let "s0" (Int 5) (Add (Var "s0") (Let "s1" (Int 5) (Var "s1")))))

    it "can uniqueify a nested let expression after recurssion" $ do
      uniquifyResult (N1 (Let "x" (Int 5) (Add (Let "y" (Int 5) (Var "y")) (Var "x"))))  `shouldBe`
        Right (N1 (Let "s0" (Int 5) (Add (Let "s1" (Int 5) (Var "s1")) (Var "s0"))))

    it "can return an Left correctly when the variable is out of scope" $ do
      uniquifyResult (N1 (Let "x" (Var "z") (Add (Var "x") (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Left "Symbol 'z' not found"

    it "can return an Left correctly when the variable is out of scope" $ do
      uniquifyResult (N1 (Let "x" (Var "x") (Add (Int 6) (Let "x" (Int 5) (Var "x")))))  `shouldBe`
        Left "Symbol 'x' not found"

  describe "Remove complex opera* pass tests:" $ do
    it "can rco on Ints" $ do
      rcoResult (N1 (Int 7)) `shouldBe` Right (N1 (Int 7))
    it "can rco on Read" $ do
      rcoResult (N1 Read) `shouldBe` Right (N1 Read)
    it "can rco on Var" $ do
      rcoResult (N1 (Var "x")) `shouldBe` Right (N1 (Var "x"))
    it "can rco on Negate Var" $ do
      rcoResult (N1 (Negate (Var "x"))) `shouldBe` Right (N1 (Negate (Var "x")))
    it "can rco on Negate Int" $ do
      rcoResult (N1 (Negate (Int 6))) `shouldBe` Right (N1 (Negate (Int 6)))
    it "can rco on Negate Negate Int" $ do
      rcoResult (N1 (Negate (Negate (Int 6)))) `shouldBe` Right (N1 (Let "s0" (Negate (Int 6)) (Negate (Var "s0"))))
    it "can rco on Negate Negate Var" $ do
      rcoResult (N1 (Negate (Negate (Var "x")))) `shouldBe` Right (N1 (Let "s0" (Negate (Var "x")) (Negate (Var "s0"))))
    it "can rco on Add Int Int" $ do
      rcoResult (N1 (Add (Int 6) (Int 7))) `shouldBe` Right (N1 (Add (Int 6) (Int 7)))
    it "can rco on Add Var Int" $ do
      rcoResult (N1 (Add (Var "x") (Int 7))) `shouldBe` Right (N1 (Add (Var "x") (Int 7)))
    it "can rco on Add Var (Negate Int)" $ do
      rcoResult (N1 (Add (Var "x") (Negate (Int 7)))) `shouldBe` Right (N1 (Let "s0" (Negate (Int 7)) (Add (Var "x") (Var "s0"))))
    it "can pass case on Add (Negate (Var \"x\")) (Negate (Int 7)))" $ do
      rcoResult (N1 (Add (Negate (Var "x")) (Negate (Int 7)))) `shouldBe`
        Right (N1
                (Let "s0" (Negate (Var "x"))
                  (Let "s1" (Negate (Int 7))
                    (Add (Var "s0") (Var "s1")))))
    it "can rco pass case on: (N1 (Add (Negate (Int 5)) (Add (Negate (Int 7)) (Negate (Int 8)))))" $ do
      rcoResult (N1 (Add (Negate (Int 5)) (Add (Negate (Int 7)) (Negate (Int 8))))) `shouldBe`
        Right (N1 (Let "s0" (Negate (Int 5))
                          (Let "s1" (Negate (Int 7))
                             (Let "s2" (Negate (Int 8))
                                (Let "s3" (Add (Var "s1") (Var "s2"))
                                  (Add (Var "s0") (Var "s3")))))))
    it "can rco on let x = 5 in x" $ do
      rcoResult (N1 (Let "x" (Int 5) (Var "x"))) `shouldBe`
        Right (N1 (Let "x" (Int 5) (Var "x")))
    it "can rco on let x = -5 in x" $ do
      rcoResult (N1 (Let "x" (Negate (Int 5)) (Var "x"))) `shouldBe`
        Right (N1 (Let "x" (Negate (Int 5)) (Var "x")))
    it "can rco on let x = -5+3 in x" $ do
      rcoResult (N1 (Let "x" (Add (Negate (Int 5)) (Int 3)) (Var "x"))) `shouldBe`
        Right (N1 (Let "x" (Let "s0" (Negate (Int 5)) (Add (Var "s0") (Int 3))) (Var "x")))
    it "can rco on let x = 5 in -(x + x)" $ do
      rcoResult (N1 (Let "x" (Int 5) (Negate (Add (Var "x") (Var "x"))))) `shouldBe`
        Right (N1 (Let "x" (Int 5) (Let "s0" (Add (Var "x") (Var "x")) (Negate (Var "s0")))))
    it "can rco on let x = 5 in -(x + -x)" $ do
      rcoResult (N1 (Let "x" (Int 5) (Negate (Add (Var "x") (Negate (Var "x")))))) `shouldBe`
        Right (N1 (Let "x" (Int 5) (Let "s0" (Negate (Var "x")) (Let "s1" (Add (Var "x") (Var "s0")) (Negate (Var "s1"))))))
