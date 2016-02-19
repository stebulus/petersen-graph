module Test.Main where

import Test.QuickCheck
import Prelude

import qualified Test.Phi as P
import qualified Test.Quaternion as Q
import qualified Test.Vector as V

main = do
  P.main
  Q.main
  V.main
