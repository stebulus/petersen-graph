module Test.Phi where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Data.ApproxNumber ((=~=))

import Phi
import Test.Function

newtype PPhi = P (Phi Number)
newtype PFPhi = PF (Phi (Number -> Number))

instance arbitraryPhi :: Arbitrary PPhi where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ P $ Phi { a: a, b: b }

instance arbitraryPFPhi :: Arbitrary PFPhi where
  arbitrary = do (F a) <- arbitrary
                 (F b) <- arbitrary
                 return $ PF $ Phi { a: a, b: b }

(~=) :: Phi Number -> Phi Number -> Result
(~=) ps@(Phi s) pt@(Phi t) =
  (s.a =~= t.a) && (s.b =~= t.b)
  <?> (show ps) ++ " not approximately equal to " ++ (show pt)
infix 4 ~=  -- same as Prelude.(==)

main = do
  -- Functor laws
  quickCheck \(P a)             -> map id a == a
  quickCheck \(P a) (F f) (F g) -> map g (map f a) == map (g <<< f) a
  -- Apply law
  quickCheck \(P a) (PF f) (PF g) -> (<<<) <$> f <*> g <*> a == f <*> (g <*> a)
  -- Applicative laws
  quickCheck \(P a)             -> (pure id) <*> a == a
  quickCheck \(P a) (PF f) (PF g) -> (pure (<<<)) <*> f <*> g <*> a == f <*> (g <*> a)
  quickCheck \a (F f)           -> (pure f) <*> (pure a) == (pure (f a) :: Phi Number)
  quickCheck \(PF f) a          -> f <*> (pure a) == (pure ($ a)) <*> f
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
