module Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Foldable (for_)
import Data.Lazy (defer)
import Data.List.Lazy (List(..), iterate, Step(..), take)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Nullable (Nullable(), toMaybe, toNullable)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElementNS)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId), elementToNode)
import Math (pi, sqrt)
import Prelude

import Quaternion (axisAngle, rotate, rotater)
import Radians hiding (scale)
import Vector

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = do
  doc <- window >>= document
  mg <- toMaybe <$> getElementById (ElementId "dots")
    (htmlDocumentToNonElementParentNode doc)
  case mg of
    Nothing -> log "error: #dots not found"
    Just g -> for_ vertices \pt -> do
        dot <- createElementNS svgns "circle" (htmlDocumentToDocument doc)
        let screen = toScreen pt
        setAttribute "cx" (show screen.u) dot
        setAttribute "cy" (show screen.v) dot
        setAttribute "r" "0.02" dot
        appendChild (elementToNode dot) (elementToNode g)

vertices :: List Vector
vertices = bot ++ top
  where fifth = axisAngle unitZ (Radians (2.0 * pi/5.0))
        top = take 5 $ iterate (rotate fifth) u
        bot = take 5 $ iterate (rotate fifth) v
        u = fromCylindrical { rho: faceCircumradius
                            , phi: Radians (-pi/2.0)
                            , z: inradius
                            }
        inradius = sqrt $ (5.0 + 2.0*(sqrt 5.0))/15.0
        faceCircumradius = sqrt (1.0 - inradius*inradius)
        opp1 = rotate (fifth <> fifth) u
        opp2 = rotate fifth opp1
        q = rotater (normalize u) (normalize (opp1 ++ opp2))
        v = rotate (q <> q) u

type Screen = { u :: Number, v :: Number }

toScreen :: Vector -> Screen
toScreen (Vector pt) = { u: t * pt.x
                       , v: t * pt.y
                       }
  where t = 1.0/(pt.z + if pt.z >= 0.0 then 1.0 else -1.0)

svgns :: Nullable String
svgns = toNullable (Just "http://www.w3.org/2000/svg")
