{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.Session
    ( sessionId
    , userId
    , expiration
    , saveSession
    , getActiveSession
    , renewSession
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

saveSession :: Connection -> Int -> User.User -> IO Session
saveSession conn sessionTime user = do
    currentTimestamp <- getPOSIXTime
    _ <- execute conn "INSERT INTO sessions (user_id, expiration) values (?, ?)" (User.userId user, round (currentTimestamp + realToFrac sessionTime) :: Int)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    return Session { sessionId = lastReturnedId, userId = User.userId user, expiration = round (currentTimestamp + 60) }

getActiveSession :: Connection -> Int -> IO Session
getActiveSession conn userId = do
    rows <- query conn "SELECT * FROM sessions WHERE user_id = ? AND expiration > UNIX_TIMESTAMP() ORDER BY expiration DESC" (Only (userId :: Int))
    case rows of
        [] -> return SessionNotFound
        ((sessionId, userId', expiration):_) -> return (Session { sessionId = sessionId, userId = userId', expiration = expiration })

renewSession :: Connection -> Int -> Session -> IO Session
renewSession conn sessionTime session = do
    rows <- query conn "SELECT * FROM sessions WHERE id = ? AND expiration > UNIX_TIMESTAMP() ORDER BY expiration DESC" (Only (sessionId session) :: Only Int)
    case rows of
        [] -> return SessionNotFound
        (sId, userId, expiration):_ -> do
            currentTimestamp <- getPOSIXTime
            _ <- execute conn "UPDATE sessions SET expiration = ? WHERE id = ?" (round (currentTimestamp + realToFrac sessionTime) :: Int, sessionId session)
            return (Session { sessionId = sId, userId = userId, expiration = expiration })