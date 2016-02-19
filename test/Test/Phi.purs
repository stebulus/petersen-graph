module Test.Phi where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Data.ApproxNumber ((=~=))

import Phi

newtype PPhi = P (Phi Number)

instance arbitraryPhi :: Arbitrary PPhi where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ P $ Phi { a: a, b: b }

(~=) :: Phi Number -> Phi Number -> Result
(~=) ps@(Phi s) pt@(Phi t) =
  (s.a =~= t.a) && (s.b =~= t.b)
  <?> (show ps) ++ " not approximately equal to " ++ (show pt)
infix 4 ~=  -- same as Prelude.(==)

main = do
  -- Semiring laws
  quickCheck \(P q) (P r) (P s) -> q+(r+s) ~= (q+r)+s
  quickCheck \(P q)             -> q+zero ~= q
  quickCheck \(P q) (P r)       -> q+r ~= r+q
  quickCheck \(P q) (P r) (P s) -> q*(r*s) ~= (q*r)*s
  quickCheck \(P q)             -> q*one ~= q
  quickCheck \(P q)             -> one*q ~= q
  quickCheck \(P q) (P r) (P s) -> q*(r+s) ~= q*r+q*s
  quickCheck \(P q) (P r) (P s) -> (q+r)*s ~= q*s+r*s
  quickCheck \(P q)             -> q*zero ~= zero
  quickCheck \(P q)             -> zero*q ~= zero
  -- Ring laws
  quickCheck \(P q)             -> q-q ~= zero
  quickCheck \(P q)             -> (zero-q)+q ~= zero
