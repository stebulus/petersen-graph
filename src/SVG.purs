module SVG (toScreen) where

import Data.Function (Fn2(), runFn2)
import DOM.Event.Types (Event())
import DOM.Node.Types (Element())

import Screen (Screen())

foreign import toScreenImpl :: Fn2 Element Event Screen

toScreen :: Element -> Event -> Screen
toScreen = runFn2 toScreenImpl
