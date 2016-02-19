module Vector where

import Data.Monoid (mempty, Monoid)
import Math (sqrt)
import Prelude

import Radians hiding (scale)

data Vector a = Vector { x :: a, y :: a, z :: a }

instance eqVector :: (Eq a) => Eq (Vector a) where
  eq (Vector u) (Vector v) = u.x == v.x
                          && u.y == v.y
                          && u.z == v.z
instance showVector :: (Show a) => Show (Vector a) where
  show (Vector v) = "Vector { x: " ++ show v.x
                        ++ ", y: " ++ show v.y
                        ++ ", z: " ++ show v.z
                        ++ " }"

instance semigroupVector :: (Semiring a) => Semigroup (Vector a) where
  append (Vector u) (Vector v) =
    Vector { x: u.x + v.x
           , y: u.y + v.y
           , z: u.z + v.z
           }
instance monoidVector :: (Semiring a) => Monoid (Vector a) where
  mempty = Vector { x: zero, y: zero, z: zero }

scale :: forall a. (Semiring a) => a -> Vector a -> Vector a
scale s (Vector v) = Vector { x: s*v.x
                            , y: s*v.y
                            , z: s*v.z
                            }

newtype UnitVector a = UnitVector (Vector a)

forgetUnit :: forall a. UnitVector a -> Vector a
forgetUnit (UnitVector v) = v

unitX :: forall a. (Semiring a) => UnitVector a
unitX = UnitVector (Vector { x: one , y: zero, z: zero })
unitY :: forall a. (Semiring a) => UnitVector a
unitY = UnitVector (Vector { x: zero, y: one , z: zero })
unitZ :: forall a. (Semiring a) => UnitVector a
unitZ = UnitVector (Vector { x: zero, y: zero, z: one  })

dot :: forall a. (Semiring a) => Vector a -> Vector a -> a
dot (Vector u) (Vector v) = u.x*v.x + u.y*v.y + u.z*v.z

normsq :: forall a. (Semiring a) => Vector a -> a
normsq v = v `dot` v

norm :: Vector Number -> Number
norm = sqrt <<< normsq

normalize :: Vector Number -> UnitVector Number
normalize v = UnitVector $ scale (one/norm v) v

type Cylindrical = { rho :: Number, phi :: Radians, z :: Number }

fromCylindrical :: Cylindrical -> Vector Number
fromCylindrical c = Vector { x: c.rho * cos c.phi
                           , y: c.rho * sin c.phi
                           , z: c.z
                           }
