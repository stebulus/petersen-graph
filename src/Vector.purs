module Vector where

import Data.Monoid (Monoid)
import Prelude

import Radians hiding (scale)

data Vector = Vector { x :: Number, y :: Number, z :: Number }

instance semigroupVector :: Semigroup Vector where
  append (Vector u) (Vector v) =
    Vector { x: u.x + v.x
           , y: u.y + v.y
           , z: u.z + v.z
           }
instance monoidVector :: Monoid Vector where
  mempty = Vector { x: 0.0, y: 0.0, z: 0.0 }

scale :: Number -> Vector -> Vector
scale s (Vector v) = Vector { x: s*v.x
                            , y: s*v.y
                            , z: s*v.z
                            }

newtype UnitVector = UnitVector Vector

forgetUnit :: UnitVector -> Vector
forgetUnit (UnitVector v) = v

unitX = UnitVector (Vector { x: 1.0, y: 0.0, z: 0.0 })
unitY = UnitVector (Vector { x: 0.0, y: 1.0, z: 0.0 })
unitZ = UnitVector (Vector { x: 0.0, y: 0.0, z: 1.0 })

dot :: Vector -> Vector -> Number
dot (Vector u) (Vector v) = u.x*v.x + u.y*v.y + u.z*v.z

norm :: Vector -> Number
norm v = v `dot` v

normalize :: Vector -> UnitVector
normalize v = UnitVector $ scale (1.0/norm v) v

type Cylindrical = { rho :: Number, phi :: Radians, z :: Number }

fromCylindrical :: Cylindrical -> Vector
fromCylindrical c = Vector { x: c.rho * cos c.phi
                           , y: c.rho * sin c.phi
                           , z: c.z
                           }
