module Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Foldable (intercalate, mconcat)
import Data.Functor.Contravariant (cmap)
import Data.List.Lazy (fromList, iterate, take)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Monoid (Monoid)
import Data.Traversable (for)
import DOM (DOM())
import DOM.Event.Event (preventDefault)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.Event.EventTypes (mousedown, mouseleave, mousemove, mouseup)
import DOM.Event.Types (EventTarget())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElementNS)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild, setTextContent)
import DOM.Node.Types (Element(), ElementId(ElementId), elementToEventTarget, elementToNode)
import Math (pi, sqrt)
import Prelude

import DOMExtra
import LogErrors
import Quaternion (axisAngle, oneU, rotate, rotater, UnitQuaternion())
import Radians hiding (scale)
import Screen
import qualified SVG as SVG
import Vector
import View

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = void $ logErrors $ do
  doc <- window >>= document
  let doc' = htmlDocumentToNonElementParentNode doc
  g <- mustGetElementById (ElementId "edges") doc'
  svg <- mustGetElementById (ElementId "petersen-graph") doc'
  world <- mustGetElementById (ElementId "world") doc'
  view <- mconcat <$> for polylines \pts -> do
    elem <- createElementNS svgns "polyline" (htmlDocumentToDocument doc)
    appendChild (elementToNode elem) (elementToNode g)
    return (rotatedPolyline elem pts)
  runView view oneU
  installDragHandlers svg world view oneU
  dragme <- mustGetElementById (ElementId "drag-me") doc'
  let hideDragMe = eventListener \evt -> do
        setAttribute "class" "dragged" dragme
        removeEventListener mousedown hideDragMe false (elementToEventTarget world)
  addEventListener mousedown hideDragMe false (elementToEventTarget world)
  setTextContent "drag me" (elementToNode dragme)

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

installDragHandlers :: forall e. Element -> Element
                    -> View (dom :: DOM | e) UnitQuaternion
                    -> UnitQuaternion
                    -> Eff (dom :: DOM | e) Unit
installDragHandlers svg target view initrot =
  let toVector evt = fromScreen (SVG.toScreen svg evt)
      target' = elementToEventTarget target
      down = eventListener \evt ->
               let draggedFrom = toVector evt
                   drag = eventListener \evt' -> do
                     preventDefault evt'
                     runView view (rotater draggedFrom (toVector evt') <> initrot)
                   drop = eventListener \evt' ->
                     let rot = rotater draggedFrom (toVector evt') <> initrot
                     in do preventDefault evt'
                           removeClass "dragging" target
                           runView view rot
                           removeEventListener mousemove drag false target'
                           removeEventListener mouseleave drop false target'
                           removeEventListener mouseup drop false target'
                           installDragHandlers svg target view rot
               in do preventDefault evt
                     addClass "dragging" target
                     addEventListener mousemove drag false target'
                     addEventListener mouseleave drop false target'
                     addEventListener mouseup drop false target'
                     removeEventListener mousedown down false target'
  in addEventListener mousedown down false target'

rotatedPolyline :: forall e. Element -> Array Vector
                -> View (dom :: DOM | e) UnitQuaternion
rotatedPolyline elem pts =
  cmap (\u -> map (toScreen <<< rotate u) pts)
       (polylineView elem)

polylineView :: forall e. Element -> View (dom :: DOM | e) (Array Screen)
polylineView elem =
  cmap (\pts -> intercalate "," $ map show $ pts >>= \pt -> [pt.u, pt.v])
       (attributeView elem "points")

toScreen :: Vector -> Screen
toScreen (Vector pt) = { u: t * pt.x
                       , v: t * pt.y
                       }
  where t = 1.0/(pt.z + if pt.z >= 0.0 then 1.0 else -1.0)

fromScreen :: Screen -> UnitVector
fromScreen s = normalize $ Vector { x: 2.0*s.u
                                  , y: 2.0*s.v
                                  , z: 1.0 - s.u*s.u - s.v*s.v
                                  }
