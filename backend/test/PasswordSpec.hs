module PasswordSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Security.Password
import qualified Data.Text.Lazy as TL

suiteSpec :: Spec
suiteSpec = do
  describe "PasswordSpec" $ do

    it "hashPassword should return a hash" $ do
      hashPassword "plainPassword" >>= (`shouldSatisfy` TL.isPrefixOf (TL.pack "$2b$10$"))

    it "comparePassword should return True" $ do
      comparePassword "$2b$10$LFMTXywinnNBX6c3tJ5TrOs8jm8v/ImYpOAfXJ4X9xRf1sWfXQtne" "plainPassword" `shouldBe` True

    it "comparePassword should return False" $ do
      comparePassword "$2b$10$LFMTXywinnNBX6c3tJ5TrOs8jm8v/ImYpOAfXJ4X9xRf1sWfXQtne" "wrongPassword" `shouldBe` False