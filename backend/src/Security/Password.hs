{-# LANGUAGE OverloadedStrings #-}

module Security.Password
    ( hashPassword
    , comparePassword
    ) where

import qualified Data.Password.Bcrypt as Bcrypt
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

hashPassword plainPassword = TL.fromStrict <$> (Bcrypt.hashPassword (Bcrypt.mkPassword (T.pack plainPassword)) >>= (pure . Bcrypt.unPasswordHash))

comparePassword :: String -> String -> Bool
comparePassword hash1 plainPassword = if compare == Bcrypt.PasswordCheckSuccess then True else False
    where
        compare = Bcrypt.checkPassword (Bcrypt.mkPassword (T.pack plainPassword)) (Bcrypt.PasswordHash (T.pack hash1))