import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Lib

main :: IO ()
main = defaultMain tests

-- AUTOMATIC TEST SUITE RUNNER - QuickCheck
-- This is not for a grade. It is to help you test your own code.

-- example tests (automatically stress-tested)

tests = [
  testGroup "My Tests" [
      testProperty "alwaysListOfSumTen's output sums to ten" checkSumsToTen,
      testProperty "alwaysFalseFunc always returns false" checkAlwaysFalse,
      testProperty "random number is always even" checkAlwaysEven,
      testProperty "you make your own test" madeYourOwnTest
      ]
  ]

-- The functions alwaysListOfSumTen and alwaysFalseFunc are defined
-- in Lib.hs. We want to test that alwaysListOfSumTen always gives
-- a list of ints that sum to 10. We want to test that alwaysFalseFunc
-- always returns False.

-- test runner tries random x values automatically
checkSumsToTen :: Integer -> Bool
checkSumsToTen x = (sum (alwaysListOfSumTen x) == 10)

checkAlwaysFalse :: Integer -> Bool
checkAlwaysFalse x = (alwaysFalseFunc x == False)

-- Will a random int always be even? This test will never pass.
-- Don't design a test like this.
checkAlwaysEven :: Integer -> Bool
checkAlwaysEven x = (even x)

-- You should want this to be True.
madeYourOwnTest :: Integer -> Bool
madeYourOwnTest x = False

