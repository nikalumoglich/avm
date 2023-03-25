import Test.Hspec

import qualified PasswordSpec as PasswordSpec
import qualified UserSpec as UserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  PasswordSpec.suiteSpec

  UserSpec.suiteSpec
