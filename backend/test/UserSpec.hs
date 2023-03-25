module UserSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Exception (evaluate)

suiteSpec :: Spec
suiteSpec = do
  describe "UserSpec" $ do

    it "saveUser should save" $ do
      pendingWith "Need to test it"

    it "saveUser should fail if user already exists" $ do
      pendingWith "Need to test it"

    it "getUserByEmail should return" $ do
      pendingWith "Need to test it"

    it "getUserByEmail should fail if user does not exist" $ do
      pendingWith "Need to test it"