module Radians where

import Data.Monoid (Monoid)
import qualified Math as Math
import Prelude

newtype Radians = Radians Number

instance semigroupRadians :: Semigroup Radians where
  append (Radians a) (Radians b) = Radians (a+b)
instance monoidRadians :: Monoid Radians where
  mempty = Radians 0.0

scale :: Number -> Radians -> Radians
scale s (Radians angle) = Radians (s*angle)

cos :: Radians -> Number
cos (Radians r) = Math.cos r

sin :: Radians -> Number
sin (Radians r) = Math.sin r
