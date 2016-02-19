module Vector where

import Data.Monoid (mempty, Monoid)
import Math (sqrt)
import Prelude

import Radians hiding (scale)

data Vector a = Vector { x :: a, y :: a, z :: a }

vector :: forall a. a -> a -> a -> Vector a
vector x y z = Vector { x: x, y: y, z: z }

instance eqVector :: (Eq a) => Eq (Vector a) where
  eq (Vector u) (Vector v) = u.x == v.x
                          && u.y == v.y
                          && u.z == v.z
instance showVector :: (Show a) => Show (Vector a) where
  show (Vector v) = "Vector { x: " ++ show v.x
                        ++ ", y: " ++ show v.y
                        ++ ", z: " ++ show v.z
                        ++ " }"

instance functorVector :: Functor Vector where
  map = apply <<< pure
instance applyVector :: Apply Vector where
  apply (Vector f) (Vector a) = vector (f.x a.x) (f.y a.y) (f.z a.z)
instance applicativeVector :: Applicative Vector where
  pure a = vector a a a

instance semigroupVector :: (Semiring a) => Semigroup (Vector a) where
  append u v = (+) <$> u <*> v
instance monoidVector :: (Semiring a) => Monoid (Vector a) where
  mempty = pure zero

scale :: forall a. (Semiring a) => a -> Vector a -> Vector a
scale s v = map (s*) v

newtype UnitVector a = UnitVector (Vector a)

forgetUnit :: forall a. UnitVector a -> Vector a
forgetUnit (UnitVector v) = v

unitX :: forall a. (Semiring a) => UnitVector a
unitX = UnitVector (vector one zero zero)
unitY :: forall a. (Semiring a) => UnitVector a
unitY = UnitVector (vector zero one zero)
unitZ :: forall a. (Semiring a) => UnitVector a
unitZ = UnitVector (vector zero zero one)

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
fromCylindrical c = vector (c.rho * cos c.phi)
                           (c.rho * sin c.phi)
                           c.z
