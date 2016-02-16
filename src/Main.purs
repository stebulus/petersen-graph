module Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Foldable (for_, intercalate)
import Data.Lazy (defer)
import Data.List.Lazy (fromList, List(..), iterate, Step(..), take)
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

import Quaternion (axisAngle, oneU, rotate, rotater, UnitQuaternion())
import Radians hiding (scale)
import Vector

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = do
  doc <- window >>= document
  mg <- toMaybe <$> getElementById (ElementId "edges")
    (htmlDocumentToNonElementParentNode doc)
  case mg of
    Nothing -> log "error: #edges not found"
    Just g -> for_ polylines \pts -> do
        elem <- createElementNS svgns "polyline" (htmlDocumentToDocument doc)
        setAttribute "points"
          (intercalate "," $ map show $
            pts >>= \pt -> let s = toScreen pt in [s.u, s.v])
          elem
        appendChild (elementToNode elem) (elementToNode g)

polylines :: Array (Array Vector)
polylines = flip map rots \rot -> map (rotate rot) polyline
  where rots = fromList $ take 5 $ iterate (fifth <>) oneU
        polyline = [ rotate fifth top1
                   , top1
                   , antitop1
                   , rotate (fifth <> fifth) antitop1
                   ]
        fifth = axisAngle unitZ (Radians (2.0 * pi/5.0))
        inradius = sqrt $ (5.0 + 2.0*(sqrt 5.0))/15.0
        faceCircumradius = sqrt (1.0 - inradius*inradius)
        top1 = fromCylindrical { rho: faceCircumradius
                               , phi: Radians (-pi/2.0)
                               , z: inradius
                               }
        oppTop1 = rotate (fifth <> fifth) top1
        oppTop2 = rotate fifth oppTop1
        q = rotater (normalize top1) (normalize (oppTop1 ++ oppTop2))
        antitop1 = rotate (q <> q) top1

type Screen = { u :: Number, v :: Number }

toScreen :: Vector -> Screen
toScreen (Vector pt) = { u: t * pt.x
                       , v: t * pt.y
                       }
  where t = 1.0/(pt.z + if pt.z >= 0.0 then 1.0 else -1.0)

svgns :: Nullable String
svgns = toNullable (Just "http://www.w3.org/2000/svg")
