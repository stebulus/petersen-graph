module Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Functor.Contravariant (cmap, Contravariant)
import Data.Foldable (for_, intercalate, mconcat)
import Data.List.Lazy (fromList, iterate, take)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Monoid (Monoid)
import Data.Nullable (Nullable(), toMaybe, toNullable)
import Data.Traversable (for)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElementNS)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element(), ElementId(ElementId), elementToEventTarget, elementToNode, NonElementParentNode())
import Math (pi, sqrt)
import Prelude

import Quaternion (axisAngle, oneU, rotate, rotater, UnitQuaternion())
import Radians hiding (scale)
import Screen
import qualified SVG as SVG
import Vector

type Model = UnitQuaternion

data View e a = View (a -> Eff (dom :: DOM | e) Unit)
instance contravariantView :: Contravariant (View e) where
  cmap f (View g) = View (g <<< f)
instance semigroupView :: Semigroup (View e a) where
  append (View f) (View g) = View \x -> f x >>= const (g x)
instance monoidView :: Monoid (View e a) where
  mempty = View \x -> return unit
runView :: forall e a. View e a -> a -> Eff (dom :: DOM | e) Unit
runView (View v) = v

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = do
  doc <- window >>= document
  mg <- toMaybe <$> getElementById (ElementId "edges")
    (htmlDocumentToNonElementParentNode doc)
  case mg of
    Nothing -> log "error: #edges not found"
    Just g -> do
      view <- mconcat <$> for polylines \pts -> do
        elem <- createElementNS svgns "polyline" (htmlDocumentToDocument doc)
        appendChild (elementToNode elem) (elementToNode g)
        return (rotatedPolyline elem pts)
      runView view oneU

rotatedPolyline :: forall e. Element -> Array Vector -> View e UnitQuaternion
rotatedPolyline elem pts =
  cmap (\u -> map (toScreen <<< rotate u) pts)
       (polylineView elem)

polylineView :: forall e. Element -> View e (Array Screen)
polylineView elem =
  cmap (\pts -> intercalate "," $ map show $ pts >>= \pt -> [pt.u, pt.v])
       (attributeView elem "points")

attributeView :: forall e. Element -> String -> View e String
attributeView elem attrname = View \attrvalue ->
  setAttribute attrname attrvalue elem

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


toScreen :: Vector -> Screen
toScreen (Vector pt) = { u: t * pt.x
                       , v: t * pt.y
                       }
  where t = 1.0/(pt.z + if pt.z >= 0.0 then 1.0 else -1.0)

svgns :: Nullable String
svgns = toNullable (Just "http://www.w3.org/2000/svg")
