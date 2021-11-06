import Test.Hspec

import AsymptoticPerfTests



main :: IO ()
main = hspec $ tests

tests :: Spec
tests = do
  describe "asymptotic perf roundtrips" $ asymptoticPerfTest
