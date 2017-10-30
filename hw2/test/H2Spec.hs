module H2Spec(main, spec) where

import Test.Hspec
import H2

main :: IO ()
main = hspec spec

binRes1 = [ [ 0 ] , [ 1 ] ]
binRes2 = [ [ 0 , 0 ] , [ 0 , 1 ] , [ 1 , 0 ] , [ 1 , 1 ] ]
binRes3 = [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
combRes1 = [ [1, 2] , [1, 3] , [1, 4] , [2, 3] , [2, 4] , [3, 4] ]
permRes1 = [ [22, 10, 5] , [22, 5, 10] , [10, 22, 5] , [10, 5, 22] , [5, 22, 10] , [5, 10, 22] ]
spec :: Spec
spec = do
  describe "H1" $ do
      it "check bin 1" $ do
            bin 1 `shouldBe` binRes1
      it "check bin 2" $ do
            bin 2 `shouldBe` binRes2
      it "check bin 3" $ do
            bin 3 `shouldBe` binRes3
      it "check comb 1" $ do
            combinations 4 2 `shouldBe` combRes1
      it "check perm 1" $ do
            permutations [22, 10, 5] `shouldBe` permRes1

