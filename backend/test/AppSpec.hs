{-# LANGUAGE OverloadedStrings #-}

module AppSpec
    ( suiteSpec
    ) where

import Test.Hspec
import App

suiteSpec :: SpecWith ()
suiteSpec = do
  describe "UserSpec" $ do

    it "getEnvOrDefault" $ do
      getEnvOrDefault "teste" "teste" >>= (`shouldBe` "teste")

    it "App" $ do
      app False >>= (`shouldBe` ())

