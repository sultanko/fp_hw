module H4Spec(main, spec) where

import Test.Hspec
import H4
import Data.Char

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "H4" $ do
      it "abParser" $ do
            runParser abParser "abcdef" `shouldBe` Just (('a','b'),"cdef")

      it "abParser" $ do
             runParser abParser "aebcdf" `shouldBe` Nothing

      it "abParser_" $ do
            runParser abParser_ "abcdef" `shouldBe` Just ((),"cdef")

      it "abParser_" $ do
             runParser abParser_ "aebcdf" `shouldBe` Nothing

      it "intPair" $ do
             runParser intPair "12 34" `shouldBe` Just ([12,34],"")
      it "intOrUppercase" $ do
              runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
      it "intOrUppercase" $ do
              runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      it "intOrUppercase" $ do
              runParser intOrUppercase "foo" `shouldBe` Nothing
      it "zeroOrMore" $ do
              runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
      it "oneOrMore" $ do
               runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
      it "zeroOrMore" $ do
              runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("","abcdeFGh")
      it "oneOrMore" $ do
               runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing
      it "ident" $ do
               runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
      it "ident" $ do
               runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
      it "ident" $ do
               runParser ident "2bad" `shouldBe` Nothing
      it "ident" $ do
               runParser ident "" `shouldBe` Nothing
      it "ident" $ do
               runParser (spaces *> posInt) " 345" `shouldBe` Just (345,"")
  
