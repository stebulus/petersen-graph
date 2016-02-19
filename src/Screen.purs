module Screen where

-- | A point in screen coordinates.
-- | `u,v` are `x,y`, but avoiding name clashes.

type Screen = { u :: Number, v :: Number }
