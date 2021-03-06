-- | The main test module
--
-- @since 0.1.0
module Main
  ( main
  ) where

import Error                 (errorSpec)
import Test.Tasty            (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hspec      (testSpec)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests))

-- The main test routine
main :: IO ()
main = do
  uTests <- unitTests
  defaultMain . opts $ testGroup "Tests" [uTests]
  where
    opts = localOption $ QuickCheckTests 5000

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
  errorUnitTests <- testSpec "Error" errorSpec
  return $ testGroup "Unit Tests" [errorUnitTests]
