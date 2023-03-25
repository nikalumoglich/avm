import Test.Hspec

import qualified PasswordSpec as PasswordSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  PasswordSpec.suiteSpec
