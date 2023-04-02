import Test.Hspec
import Database.MySQL.Simple

import qualified AppSpec
import qualified PasswordSpec
import qualified JwtSpec
import qualified UserSpec
import qualified SessionSpec
import qualified PermissionSpec
import qualified SignUpHandlerSpec
import qualified SignInHandlerSpec
import qualified LoggedHandlerSpec


createDbConn :: IO Connection
createDbConn = connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm_test" })

-- get database params from test env
main :: IO ()
main = do
  conn <- createDbConn
  hspec (spec conn)

spec conn = do

  AppSpec.suiteSpec
  
  PasswordSpec.suiteSpec
  JwtSpec.suiteSpec

  UserSpec.suiteSpec conn
  SessionSpec.suiteSpec conn
  PermissionSpec.suiteSpec conn

  SignUpHandlerSpec.suiteSpec conn
  SignInHandlerSpec.suiteSpec
  LoggedHandlerSpec.suiteSpec conn
