module Quaternion where

import Data.Monoid (Monoid)
import Math (pow, sqrt)
import Prelude

import qualified Radians as R
import qualified Vector as V

data Quaternion a = Quaternion { r :: a, i :: a, j :: a, k :: a }

instance eqQuaternion :: (Eq a) => Eq (Quaternion a) where
  eq (Quaternion a) (Quaternion b) =
    a.r == b.r && a.i == b.i && a.j == b.j && a.k == b.k

instance showQuaternion :: (Show a) => Show (Quaternion a) where
  show (Quaternion q) = "Quaternion { r: " ++ show q.r
                               ++ " , i: " ++ show q.i
                               ++ " , j: " ++ show q.j
                               ++ " , k: " ++ show q.k
                               ++ " }"

instance semiringQuarternion :: (Ring a) => Semiring (Quaternion a) where
  add (Quaternion a) (Quaternion b) =
    Quaternion { r: a.r + b.r
               , i: a.i + b.i
               , j: a.j + b.j
               , k: a.k + b.k
               }
  zero = Quaternion { r: zero, i: zero, j: zero, k: zero }
  mul (Quaternion a) (Quaternion b) =
    Quaternion { r: a.r*b.r - a.i*b.i - a.j*b.j - a.k*b.k
               , i: a.r*b.i + a.i*b.r + a.j*b.k - a.k*b.j
               , j: a.r*b.j + a.j*b.r + a.k*b.i - a.i*b.k
               , k: a.r*b.k + a.k*b.r + a.i*b.j - a.j*b.i
               }
  one = Quaternion { r: one, i: zero, j: zero, k: zero }
instance ringQuaternion :: (Ring a) => Ring (Quaternion a) where
  sub (Quaternion a) (Quaternion b) =
    Quaternion { r: a.r - b.r
               , i: a.i - b.i
               , j: a.j - b.j
               , k: a.k - b.k
               }
instance modulosemiringQuaternion :: (DivisionRing a) => ModuloSemiring (Quaternion a) where
  div n d = scale (one/normsq d) (n * conjugate d)
  mod n d = zero
instance divisionringQuaternion :: (DivisionRing a) => DivisionRing (Quaternion a)

scale :: forall a. (Semiring a) => a -> Quaternion a -> Quaternion a
scale s (Quaternion q) =
    Quaternion { r: s*q.r, i: s*q.i, j: s*q.j, k: s*q.k }

conjugate :: forall a. (Ring a) => Quaternion a -> Quaternion a
conjugate (Quaternion q) = Quaternion { r: q.r
                                      , i: negate q.i
                                      , j: negate q.j
                                      , k: negate q.k
                                      }

normsq :: forall a. (Semiring a) => Quaternion a -> a
normsq (Quaternion q) = q.r*q.r + q.i*q.i + q.j*q.j + q.k*q.k

norm :: Quaternion Number -> Number
norm = sqrt <<< normsq

fromVector :: forall a. (Semiring a) => V.Vector a -> Quaternion a
fromVector (V.Vector p) = Quaternion { r: zero, i: p.x, j: p.y, k: p.z }

scalarPart :: forall a. Quaternion a -> a
scalarPart (Quaternion q) = q.r

vectorPart :: forall a. Quaternion a -> V.Vector a
vectorPart (Quaternion q) = V.vector q.i q.j q.k

newtype UnitQuaternion a = UnitQuaternion (Quaternion a)

oneU :: forall a. (Ring a) => UnitQuaternion a
oneU = UnitQuaternion one

normalize :: Quaternion Number -> UnitQuaternion Number
normalize q = UnitQuaternion (scale (1.0/norm q) q)

instance semigroupUnitQuaternion :: (Ring a) => Semigroup (UnitQuaternion a) where
  append (UnitQuaternion u) (UnitQuaternion v) = UnitQuaternion (u*v)

instance monoidUnitQuaternion :: (Ring a) => Monoid (UnitQuaternion a) where
  mempty = oneU

rotate :: forall a. (Ring a) => UnitQuaternion a -> V.Vector a -> V.Vector a
rotate (UnitQuaternion u) p = vectorPart (u * (fromVector p) * (conjugate u))

rotater :: V.UnitVector Number -> V.UnitVector Number -> UnitQuaternion Number
rotater (V.UnitVector from) (V.UnitVector to) =
  normalize (one - (fromVector to)*(fromVector from))

axisAngle :: V.UnitVector Number -> R.Radians -> UnitQuaternion Number
axisAngle (V.UnitVector (V.Vector axis)) angle =
  UnitQuaternion (Quaternion { r: R.cos (R.scale 0.5 angle)
                             , i: axis.x * s
                             , j: axis.y * s
                             , k: axis.z * s
                             })
  where s = R.sin (R.scale 0.5 angle)
