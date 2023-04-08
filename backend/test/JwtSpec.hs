module JwtSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Security.Jwt
import qualified Model.Session as Session

suiteSpec :: Spec
suiteSpec = do
  describe "JwtSpec" $ do

    it "Should encode session" $ do
      let session = Session.Session { Session.sessionId = 1, Session.userId = 2, Session.expiration = 3 }
      encodeSession "secret" session >>= (`shouldBe` (JWTToken "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHBpcmF0aW9uIjozLCJzZXNzaW9uSWQiOjEsInVzZXJJZCI6Mn0.3qg2hpfojqECKETvsnCgDsa3LnWRs_yZSusILLywfnQ"))

    it "Should decode valid session" $ do
      decodeSession "secret" "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHBpcmF0aW9uIjozLCJzZXNzaW9uSWQiOjEsInVzZXJJZCI6Mn0.3qg2hpfojqECKETvsnCgDsa3LnWRs_yZSusILLywfnQ" `shouldBe` Session.Session { Session.sessionId = 1, Session.userId = 2, Session.expiration = 3 }

    it "Should not decode invalid session" $ do
      decodeSession "secret" "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHBpcmF0aW9uIjozLCJzZXNzaW9uSWQiOjEsInVzZXJJZCI6Mn0.3qg2hpfojqECKETvsnCgDsa3LnWRs_yZSusILLywfnR" `shouldBe` Session.SessionNotFound
