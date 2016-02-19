module Test.Vector where

import Math (abs)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Data.ApproxNumber ((=~=))
import Prelude

import Test.Function
import Vector

newtype VVector = V (Vector Number)
newtype UVector = UV (UnitVector Number)
newtype VFVector = VF (Vector (Number -> Number))

instance arbitraryVector :: Arbitrary VVector where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ V $ Vector { x: x, y: y, z: z }
instance arbitraryUnitVector :: Arbitrary UVector where
  arbitrary = do (V v) <- arbitrary
                 return $ UV $ normalize v
instance arbitraryVFVector :: Arbitrary VFVector where
  arbitrary = do (F x) <- arbitrary
                 (F y) <- arbitrary
                 (F z) <- arbitrary
                 return $ VF $ Vector { x: x, y: y, z: z }

(~=) :: Vector Number -> Vector Number -> Result
(~=) va@(Vector a) vb@(Vector b) =
  (a.x =~= b.x)
  && (a.y =~= b.y)
  && (a.z =~= b.z)
  <?> (show va) ++ " not approximately equal to " ++ (show vb)
infix 4 ~=  -- same as Prelude.(==)

main = do
  -- Functor laws
  quickCheck \(V a)             -> map id a == a
  quickCheck \(V a) (F f) (F g) -> map g (map f a) == map (g <<< f) a
  -- Apply law
  quickCheck \(V a) (VF f) (VF g) -> (<<<) <$> f <*> g <*> a == f <*> (g <*> a)
  -- Applicative laws
  quickCheck \(V a)             -> (pure id) <*> a == a
  quickCheck \(V a) (VF f) (VF g) -> (pure (<<<)) <*> f <*> g <*> a == f <*> (g <*> a)
  quickCheck \a (F f)           -> (pure f) <*> (pure a) == (pure (f a) :: Vector Number)
  quickCheck \(VF f) a          -> f <*> (pure a) == (pure ($ a)) <*> f
  -- Semigroup laws
  quickCheck \(V u) (V v) (V w) -> u++(v++w) ~= (u++v)++w
  -- abelian semigroup
  quickCheck \(V u) (V v)       -> u++v ~= v++u
  -- scalar multiplication
  quickCheck \s t (V v)         -> scale (s+t) v ~= scale s v ++ scale t v
  quickCheck \s t (V v)         -> scale (s*t) v ~= scale s (scale t v)
  quickCheck \s (V u) (V v)     -> scale s (u++v) ~= scale s u ++ scale s v
  -- dot product and norm
  quickCheck \(V u) (V v)       -> u `dot` v =~= v `dot` u
  quickCheck \(V v)             -> v `dot` v =~= normsq v
  quickCheck \(V u) (V v) (V w) -> u `dot` (v++w) =~= u `dot` v + u `dot` w
  quickCheck \s (V v)           -> norm (scale s v) =~= abs s * norm v
  quickCheck \s (V u) (V v)     -> (scale s u) `dot` v =~= s*(u `dot` v)
