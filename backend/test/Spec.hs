import Test.Hspec
import Database.MySQL.Simple

import qualified AppSpec
import qualified PasswordSpec
import qualified JwtSpec
import qualified UserSpec
import qualified SessionSpec
import qualified PermissionSpec
import qualified ImageSpec
import qualified DimensionSpec
import qualified SignUpHandlerSpec
import qualified SignInHandlerSpec
import qualified LoggedHandlerSpec
import qualified ProductsHandlerSpec
import System.Environment
import Control.Exception
import qualified System.IO.Error

getEnvOrDefault :: String -> String -> IO String
getEnvOrDefault name defaultValue = getEnv name `catch` handleIsDoesNotExistError defaultValue

handleIsDoesNotExistError :: String -> System.IO.Error.IOError -> IO String
handleIsDoesNotExistError defaultValue _ = return defaultValue

createDbConn :: String -> String -> String -> String -> IO Connection
createDbConn host user password database = connect (defaultConnectInfo { connectHost = host, connectUser = user, connectPassword = password, connectDatabase = database })

-- get database params from test env
main :: IO ()
main = do
  host <- getEnvOrDefault "DB_HOST" "127.0.0.1"
  user <- getEnvOrDefault "DB_USER" "haskelluser"
  password <- getEnvOrDefault "DB_PASSWORD" "haskellpassword"
  database <- getEnvOrDefault "DB_NAME" "avm"
  bucket <- getEnvOrDefault "AWS_S3_BUCKET" "tiozao-avm"
  conn <- createDbConn host user password database
  hspec (spec conn host database user password bucket)

spec conn host database user password bucket = do
  AppSpec.suiteSpec
  
  PasswordSpec.suiteSpec
  JwtSpec.suiteSpec

  UserSpec.suiteSpec conn
  SessionSpec.suiteSpec conn
  PermissionSpec.suiteSpec conn
  ImageSpec.suiteSpec conn bucket
  DimensionSpec.suiteSpec conn bucket

  SignUpHandlerSpec.suiteSpec conn host database user password bucket
  SignInHandlerSpec.suiteSpec host database user password bucket
  LoggedHandlerSpec.suiteSpec conn host database user password bucket
  ProductsHandlerSpec.suiteSpec conn host database user password bucket