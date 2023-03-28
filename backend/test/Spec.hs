import Test.Hspec

import qualified PasswordSpec
import qualified JwtSpec
import qualified UserSpec
import qualified SessionSpec
import qualified SignUpHandlerSpec
import qualified SignInHandlerSpec

-- get database params from test env
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  PasswordSpec.suiteSpec

  UserSpec.suiteSpec

  JwtSpec.suiteSpec

  SessionSpec.suiteSpec

  SignUpHandlerSpec.suiteSpec

  SignInHandlerSpec.suiteSpec
