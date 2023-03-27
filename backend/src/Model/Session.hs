{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Session
    ( sessionId
    , userId
    , expiration
    , saveSession
    , getActiveSession
    , Session ( Session, SessionNotFound )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import Data.Time.Clock.POSIX
import qualified Data.Aeson as Aeson
import qualified Model.User as User

data Session = Session
  { sessionId :: Int
  , userId :: Int
  , expiration :: Int
  } | SessionNotFound
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Session
instance Aeson.ToJSON Session

saveSession :: Connection -> User.User -> IO Session
saveSession conn user = do
    currentTimestamp <- getPOSIXTime
    _ <- execute conn "INSERT INTO sessions (user_id, expiration) values (?, ?)" (User.userId user, round (currentTimestamp + 60) :: Int) -- 60 seconds, should parameterize it
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    return Session { sessionId = lastReturnedId, userId = User.userId user, expiration = round (currentTimestamp + 60) }

getActiveSession :: Connection -> User.User -> IO Session
getActiveSession conn user = do
    rows <- query conn "SELECT * FROM sessions WHERE user_id = ? AND exp < UNIX_TIMESTAMP()" (Only (User.userId user) :: Only Int)
    case rows of
        [] -> return SessionNotFound
        ((sessionId, userId, expiration):_) -> return (Session { sessionId = sessionId, userId = userId, expiration = expiration })