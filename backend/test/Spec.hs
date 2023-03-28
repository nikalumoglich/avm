import Test.Hspec

import qualified PasswordSpec
import qualified JwtSpec
import qualified UserSpec
import qualified SessionSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  PasswordSpec.suiteSpec

  UserSpec.suiteSpec

  JwtSpec.suiteSpec

  SessionSpec.suiteSpec
