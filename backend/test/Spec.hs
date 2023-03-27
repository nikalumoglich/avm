import Test.Hspec

import qualified PasswordSpec
import qualified UserSpec
import qualified JwtSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  PasswordSpec.suiteSpec

  UserSpec.suiteSpec

  JwtSpec.suiteSpec
