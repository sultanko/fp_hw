module H1Spec(main, spec) where

import Test.Hspec
import H1

main :: IO ()
main = hspec spec

exprDivError = (Const 3) `Division` (Const 0)
exprDivOk = (Const 6) `Division` (Const 2)
exprAdd = (Const 6) `Sum` (Const 2)
exprPower = (Const 2) `Power` (Const 3)
exprPowerError = (Const 2) `Power` (Const (-3))

spec :: Spec
spec = do
  describe "H1" $ do
      it "eval const" $ do
            eval (Const 3) `shouldBe` (Right 3)

      it "eval Add" $ do
            eval exprAdd `shouldBe` (Right 8)
      it "eval power" $ do
            eval exprPower `shouldBe` (Right 8)
      it "eval div" $ do
            eval exprDivOk `shouldBe` (Right 3)
      it "eval div error" $ do
            eval exprDivError `shouldBe` (Left (ArithmeticError "Division by zero"))
      it "eval power error" $ do
            eval exprPowerError `shouldBe` (Left (ArithmeticError "Negative power"))
