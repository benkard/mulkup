import qualified Mulkup.ConfigSpec
import Mulkup.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [Mulkup.ConfigSpec.cases]
