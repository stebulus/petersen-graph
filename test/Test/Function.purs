module Test.Function where

import Test.QuickCheck.Arbitrary
import Prelude

newtype FFunction = F (Number -> Number)

instance arbitraryFFunction :: Arbitrary FFunction where
  arbitrary = F <$> arbitrary
