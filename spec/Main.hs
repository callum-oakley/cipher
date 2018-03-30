module Main where

import Test.Hspec
import Test.QuickCheck

import Cipher

main :: IO ()
main = hspec $ do
  describe "caesar" $ do
    it "and unCaesar are inverse" $ do
      property $ \n s -> unCaesar n (caesar n s) `shouldBe` s
  describe "vigenere" $ do
    it "and unVigenere are inverse" $ do
      property $ \p s -> unVigenere p (vigenere p s) `shouldBe` s
