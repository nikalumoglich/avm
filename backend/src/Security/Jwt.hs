{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Security.Jwt
    ( encodeSession
    , decodeSession
    , token
    , JWTToken (JWTToken)
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import Web.JWT
import GHC.Generics
import qualified Model.Session as Session

data JWTToken = JWTToken {
    token :: String
} deriving (Generic, Eq, Show)

instance Aeson.FromJSON JWTToken
instance Aeson.ToJSON JWTToken

encodeSession :: String -> Session.Session -> IO JWTToken
encodeSession secret session = do
    let
        cs = mempty { -- mempty returns a default JWTClaimsSet
          unregisteredClaims = ClaimsMap $ Map.fromList [
            ("sessionId", Aeson.Number (intToScientific (toInteger $ Session.sessionId session))),
            ("userId", Aeson.Number (intToScientific (toInteger $ Session.userId session))),
            ("expiration", Aeson.Number (intToScientific (toInteger $ Session.expiration session)))
            ]
        }
        key = hmacSecret . T.pack $ secret
    let encoded = encodeSigned key mempty cs
    return (JWTToken (T.unpack encoded))

decodeSession :: String -> String -> Session.Session
decodeSession secret encodedJwt = fromJust $ decodeSession' secret encodedJwt

decodeSession' :: String -> String -> Maybe Session.Session
decodeSession' secret encodedJwt = do
    let input = T.pack encodedJwt
    let mJwt = decodeAndVerifySignature (toVerify . hmacSecret . T.pack $ secret) input
    case mJwt of
        Nothing -> return Session.SessionNotFound
        Just verifiedJWT -> do
            let dataMap = unClaimsMap (unregisteredClaims (claims verifiedJWT))
            sessionIdValue <- Map.lookup "sessionId" dataMap >>= scientificToInt
            userIdValue <- Map.lookup "userId" dataMap >>= scientificToInt
            expirationValue <- Map.lookup "expiration" dataMap >>= scientificToInt
            return Session.Session { Session.sessionId = sessionIdValue, Session.userId = userIdValue, Session.expiration = expirationValue }

intToScientific :: Integer -> Scientific.Scientific
intToScientific x = Scientific.scientific x 0

scientificToInt :: (Integral i, Bounded i) => Aeson.Value -> Maybe i
scientificToInt (Aeson.Number x) = Scientific.toBoundedInteger x