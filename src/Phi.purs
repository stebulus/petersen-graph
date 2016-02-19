module Phi where

import Data.Int (toNumber)
import Math (sqrt)
import Prelude

-- | Numbers of the form `a + b*phi`

data Phi a = Phi { a :: a, b :: a }

phi :: forall a. (Semiring a) => Phi a
phi = Phi { a: zero, b: one }

phiNumber :: Number
phiNumber = (1.0 + sqrt 5.0)/2.0

fromPhiNumber :: Phi Number -> Number
fromPhiNumber (Phi s) = s.a + phiNumber*s.b

fromPhiInt :: Phi Int -> Number
fromPhiInt = fromPhiNumber <<< map toNumber

instance eqPhi :: (Eq a) => Eq (Phi a) where
  eq (Phi s) (Phi t) = s.a == t.a && s.b == t.b

instance showPhi :: (Show a) => Show (Phi a) where
  show (Phi s) = "Phi { a: " ++ show s.a ++ ", b: " ++ show s.b ++ " }"

instance functorPhi :: Functor Phi where
  map = apply <<< pure

instance applyPhi :: Apply Phi where
  apply (Phi f) (Phi x) = Phi { a: f.a x.a, b: f.b x.b }

instance applicativePhi :: Applicative Phi where
  pure x = Phi { a: x, b: x }

instance semiringPhi :: (Semiring a) => Semiring (Phi a) where
  add s t = add <$> s <*> t
  zero = pure zero
  mul (Phi s) (Phi t) = Phi { a: s.a*t.a + bb
                            , b: s.a*t.b + s.b*t.a + bb
                            }
    where bb = s.b*t.b
  one = Phi { a: one, b: zero }

instance ringPhi :: (Ring a) => Ring (Phi a) where
  sub s t = sub <$> s <*> t
