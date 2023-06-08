{-# LANGUAGE DeriveGeneric #-}

module Errors
    ( invalidJsonError
    , invalidSessionError
    , userNotFoundError
    , wrongPasswordError
    , userAlreadyExistError
    , Error (Error)
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data Error = Error
  { message :: String
  , code :: Int
  } deriving (Show, Generic)

instance Aeson.FromJSON Error
instance Aeson.ToJSON Error

invalidJsonError :: Error
invalidJsonError = Error {
    message = "Invalid Json format",
    code = 1
}

invalidSessionError :: Error
invalidSessionError = Error {
    message = "Invalid Session",
    code = 2
}

userNotFoundError :: Error
userNotFoundError = Error {
    message = "User not found",
    code = 3
}

wrongPasswordError :: Error
wrongPasswordError = Error {
    message = "Wrong password",
    code = 4
}

userAlreadyExistError :: Error
userAlreadyExistError = Error {
    message = "User already exist",
    code = 5
}