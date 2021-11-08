module Main
  ( main,
  )
where

import qualified Spec.Test
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Distributor"
    [ Spec.Test.simpleDistributionTest
    ]