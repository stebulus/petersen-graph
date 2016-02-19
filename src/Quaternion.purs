module Quaternion where

import Data.Monoid (Monoid)
import Math (pow, sqrt)
import Prelude

import qualified Radians as R
import qualified Vector as V

data Quaternion a = Quaternion { r :: a, i :: a, j :: a, k :: a }

quaternion :: forall a. a -> a -> a -> a -> Quaternion a
quaternion r i j k = Quaternion { r: r, i: i, j: j, k: k }

instance eqQuaternion :: (Eq a) => Eq (Quaternion a) where
  eq (Quaternion a) (Quaternion b) =
    a.r == b.r && a.i == b.i && a.j == b.j && a.k == b.k

instance showQuaternion :: (Show a) => Show (Quaternion a) where
  show (Quaternion q) = "Quaternion { r: " ++ show q.r
                               ++ " , i: " ++ show q.i
                               ++ " , j: " ++ show q.j
                               ++ " , k: " ++ show q.k
                               ++ " }"

instance functorQuaternion :: Functor Quaternion where
  map = apply <<< pure

instance applyQuaternion :: Apply Quaternion where
  apply (Quaternion f) (Quaternion a) =
    quaternion (f.r a.r) (f.i a.i) (f.j a.j) (f.k a.k)

instance applicativeQuaternion :: Applicative Quaternion where
  pure a = quaternion a a a a

instance semiringQuaternion :: (Ring a) => Semiring (Quaternion a) where
  add a b = add <$> a <*> b
  zero = pure zero
  mul (Quaternion a) (Quaternion b) =
    quaternion (a.r*b.r - a.i*b.i - a.j*b.j - a.k*b.k)
               (a.r*b.i + a.i*b.r + a.j*b.k - a.k*b.j)
               (a.r*b.j + a.j*b.r + a.k*b.i - a.i*b.k)
               (a.r*b.k + a.k*b.r + a.i*b.j - a.j*b.i)
  one = quaternion one zero zero zero

instance ringQuaternion :: (Ring a) => Ring (Quaternion a) where
  sub a b = sub <$> a <*> b

instance modulosemiringQuaternion :: (DivisionRing a) => ModuloSemiring (Quaternion a) where
  div n d = scale (one/normsq d) (n * conjugate d)
  mod n d = zero

instance divisionringQuaternion :: (DivisionRing a) => DivisionRing (Quaternion a)

scale :: forall a. (Semiring a) => a -> Quaternion a -> Quaternion a
scale s q = map (s*) q

conjugate :: forall a. (Ring a) => Quaternion a -> Quaternion a
conjugate (Quaternion q) =
  quaternion q.r (negate q.i) (negate q.j) (negate q.k)

normsq :: forall a. (Semiring a) => Quaternion a -> a
normsq (Quaternion q) = q.r*q.r + q.i*q.i + q.j*q.j + q.k*q.k

norm :: Quaternion Number -> Number
norm = sqrt <<< normsq

fromVector :: forall a. (Semiring a) => V.Vector a -> Quaternion a
fromVector (V.Vector p) = quaternion zero p.x p.y p.z

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
  UnitQuaternion $ quaternion c (axis.x * s) (axis.y * s) (axis.z * s)
  where s = R.sin half
        c = R.cos half
        half = R.scale 0.5 angle
