module Test.Quaternion where

import Data.Foldable (traverse_)
import Math (pow)
import Prelude
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Data.ApproxNumber ((=~=))

import Quaternion
import qualified Vector as V
import Test.Vector (VVector(..), UVector(..))
import qualified Test.Vector as TV

newtype AQuaternion = A (Quaternion Number)
newtype UQuaternion = U (UnitQuaternion Number)

instance arbitraryQuaternion :: Arbitrary AQuaternion where
  arbitrary = do r <- arbitrary
                 i <- arbitrary
                 j <- arbitrary
                 k <- arbitrary
                 return $ A $ Quaternion { r: r, i: i, j: j, k: k }
instance arbitraryUnitQuaternion :: Arbitrary UQuaternion where
  arbitrary = do (A q) <- arbitrary
                 return $ U $ normalize q

(~=) :: Quaternion Number -> Quaternion Number -> Result
(~=) qa@(Quaternion a) qb@(Quaternion b) =
  (a.r =~= b.r)
  && (a.i =~= b.i)
  && (a.j =~= b.j)
  && (a.k =~= b.k)
  <?> (show qa) ++ " not approximately equal to " ++ (show qb)
infix 4 ~=  -- same as Prelude.(==)

main = do
  -- Semiring laws
  quickCheck \(A q) (A r) (A s) -> q+(r+s) ~= (q+r)+s
  quickCheck \(A q)             -> q+zero ~= q
  quickCheck \(A q) (A r)       -> q+r ~= r+q
  quickCheck \(A q) (A r) (A s) -> q*(r*s) ~= (q*r)*s
  quickCheck \(A q)             -> q*one ~= q
  quickCheck \(A q)             -> one*q ~= q
  quickCheck \(A q) (A r) (A s) -> q*(r+s) ~= q*r+q*s
  quickCheck \(A q) (A r) (A s) -> (q+r)*s ~= q*s+r*s
  quickCheck \(A q)             -> q*zero ~= zero
  quickCheck \(A q)             -> zero*q ~= zero
  -- Ring laws
  quickCheck \(A q)             -> q-q ~= zero
  quickCheck \(A q)             -> (zero-q)+q ~= zero
  -- ModuloSemiring laws
  quickCheck \(A q) (A r)       -> q/r*r + (q `mod` r) ~= q
  -- scalar multiplication
  quickCheck \a (A q) (A r)     -> scale a (q+r) ~= (scale a q) + (scale a r)
  quickCheck \a b (A q)         -> scale (a+b) q ~= (scale a q) + (scale b q)
  quickCheck \a b (A q)         -> scale (a*b) q ~= scale a (scale b q)
  -- conjugation and norm
  quickCheck \(A q)             -> norm q >= 0.0
  quickCheck $                     norm one == 1.0
  quickCheck $                     norm zero == 0.0
  quickCheck \(A q)             -> norm q =~= norm (conjugate q)
  quickCheck \(A q)             -> q*(conjugate q) ~= scale (normsq q) one
  quickCheck \(A q)             -> q+(conjugate q) ~= scale (2.0 * scalarPart q) one
  quickCheck \(A q)             -> q-(conjugate q)
                                    ~= scale 2.0 (fromVector (vectorPart q))
  -- DivisionRing laws
  quickCheck \(A q)             -> (one/q)*q ~= one
  -- rotation
  quickCheck \(U u) (V v)       -> V.norm v =~= V.norm (rotate u v)
  quickCheck \(UV u) (UV v)     -> rotate (rotater u v) (V.forgetUnit u)
                                     TV.~= V.forgetUnit v
