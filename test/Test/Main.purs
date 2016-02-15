module Test.Main where

import Test.QuickCheck
import Prelude

import qualified Test.Quaternion as Q
import qualified Test.Vector as V

main = do
  Q.main
  V.main
