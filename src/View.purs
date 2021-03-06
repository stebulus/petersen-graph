module View where

import Control.Monad.Eff (Eff())
import Data.Functor.Contravariant (cmap, Contravariant)
import Data.Monoid (Monoid)
import Prelude

-- | A view as in "model-view-controller".
-- | `a` is the type of data that it presents.

data View e a = View (a -> Eff e Unit)

runView :: forall e a. View e a -> a -> Eff e Unit
runView (View v) = v

instance contravariantView :: Contravariant (View e) where
  cmap f (View g) = View (g <<< f)

-- | Combine views of the same data with `(<>)`.

instance semigroupView :: Semigroup (View e a) where
  append (View f) (View g) = View \x -> f x >>= const (g x)

instance monoidView :: Monoid (View e a) where
  mempty = View \x -> return unit
