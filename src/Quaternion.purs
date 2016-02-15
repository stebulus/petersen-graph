module Quaternion where

import Data.Monoid (Monoid)
import Math (pow, sqrt)
import Prelude

import qualified Radians as R
import qualified Vector as V

data Quaternion = Quaternion { r :: Number
                             , i :: Number
                             , j :: Number
                             , k :: Number
                             }
instance eqQuaternion :: Eq Quaternion where
  eq (Quaternion a) (Quaternion b) =
    a.r == b.r && a.i == b.i && a.j == b.j && a.k == b.k
instance showQuaternion :: Show Quaternion where
  show (Quaternion q) = "Quaternion { r: " ++ show q.r
                               ++ " , i: " ++ show q.i
                               ++ " , j: " ++ show q.j
                               ++ " , k: " ++ show q.k
                               ++ " }"

instance semiringQuarternion :: Semiring Quaternion where
  add (Quaternion a) (Quaternion b) =
    Quaternion { r: a.r + b.r
               , i: a.i + b.i
               , j: a.j + b.j
               , k: a.k + b.k
               }
  zero = Quaternion { r: 0.0, i: 0.0, j: 0.0, k: 0.0 }
  mul (Quaternion a) (Quaternion b) =
    Quaternion { r: a.r*b.r - a.i*b.i - a.j*b.j - a.k*b.k
               , i: a.r*b.i + a.i*b.r + a.j*b.k - a.k*b.j
               , j: a.r*b.j + a.j*b.r + a.k*b.i - a.i*b.k
               , k: a.r*b.k + a.k*b.r + a.i*b.j - a.j*b.i
               }
  one = Quaternion { r: 1.0, i: 0.0, j: 0.0, k: 0.0 }
instance ringQuaternion :: Ring Quaternion where
  sub (Quaternion a) (Quaternion b) =
    Quaternion { r: a.r - b.r
               , i: a.i - b.i
               , j: a.j - b.j
               , k: a.k - b.k
               }
instance modulosemiringQuaternion :: ModuloSemiring Quaternion where
  div n d = scale (1.0/normsq d) (n * conjugate d)
  mod n d = zero
instance divisionringQuaternion :: DivisionRing Quaternion

scale :: Number -> Quaternion -> Quaternion
scale s (Quaternion q) =
    Quaternion { r: s*q.r
               , i: s*q.i
               , j: s*q.j
               , k: s*q.k
               }

conjugate :: Quaternion -> Quaternion
conjugate (Quaternion q) = Quaternion { r: q.r, i: -q.i, j: -q.j, k: -q.k }

normsq :: Quaternion -> Number
normsq (Quaternion q) = q.r*q.r + q.i*q.i + q.j*q.j + q.k*q.k

norm :: Quaternion -> Number
norm = sqrt <<< normsq

newtype UnitQuaternion = UnitQuaternion Quaternion

normalize :: Quaternion -> UnitQuaternion
normalize q = UnitQuaternion (scale (1.0/norm q) q)

instance semigroupUnitQuaternion :: Semigroup UnitQuaternion where
  append (UnitQuaternion u) (UnitQuaternion v) = UnitQuaternion (u*v)
instance monoidUnitQuaternion :: Monoid UnitQuaternion where
  mempty = UnitQuaternion one

rotate :: UnitQuaternion -> V.Vector -> V.Vector
rotate (UnitQuaternion u) p = vectorPart (u * (fromVector p) * (conjugate u))

rotater :: V.UnitVector -> V.UnitVector -> UnitQuaternion
rotater (V.UnitVector from) (V.UnitVector to) =
  normalize (one - (fromVector from)*(fromVector to))

fromVector :: V.Vector -> Quaternion
fromVector (V.Vector p) = Quaternion { r: 0.0, i: p.x, j: p.y, k: p.z }

scalarPart :: Quaternion -> Number
scalarPart (Quaternion q) = q.r

vectorPart :: Quaternion -> V.Vector
vectorPart (Quaternion q) = V.Vector { x: q.i, y: q.j, z: q.k }

axisAngle :: V.UnitVector -> R.Radians -> UnitQuaternion
axisAngle (V.UnitVector (V.Vector axis)) angle =
  UnitQuaternion (Quaternion { r: R.cos (R.scale 0.5 angle)
                             , i: axis.x * s
                             , j: axis.y * s
                             , k: axis.z * s
                             })
  where s = R.sin (R.scale 0.5 angle)
