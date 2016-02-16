module Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (catchException, error, EXCEPTION(), message, throwException)
import Data.Functor.Contravariant (cmap, Contravariant)
import Data.Foldable (for_, intercalate, mconcat)
import Data.List.Lazy (fromList, iterate, take)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Monoid (Monoid)
import Data.Nullable (Nullable(), toMaybe, toNullable)
import Data.Traversable (for)
import DOM (DOM())
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.Event.EventTypes (mousedown, mouseleave, mousemove, mouseup)
import DOM.Event.Types (EventTarget())
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

mainM :: Eff (dom :: DOM, console :: CONSOLE, err :: EXCEPTION) Unit
mainM = do
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
  installDragHandlers svg (elementToEventTarget world) view oneU

mustGetElementById :: forall e. ElementId -> NonElementParentNode
                   -> Eff (dom :: DOM, err :: EXCEPTION | e) Element
mustGetElementById elemid@(ElementId idstr) parent = do
  melem <- toMaybe <$> getElementById elemid parent
  case melem of
    Nothing -> throwException (error ("#" ++ idstr ++ " not found"))
    Just elem -> return elem

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

installDragHandlers :: forall e. Element -> EventTarget -> View (console :: CONSOLE | e) UnitQuaternion -> UnitQuaternion -> Eff (dom :: DOM, console :: CONSOLE | e) Unit
installDragHandlers svg target view initrot =
  let toVector evt = fromScreen (SVG.toScreen svg evt)
      down = eventListener \evt ->
               let draggedFrom = toVector evt
                   drag = eventListener \evt' ->
                     runView view (rotater draggedFrom (toVector evt') <> initrot)
                   drop = eventListener \evt' ->
                     let rot = rotater draggedFrom (toVector evt') <> initrot
                     in do runView view rot
                           removeEventListener mousemove drag false target
                           removeEventListener mouseleave drop false target
                           removeEventListener mouseup drop false target
                           installDragHandlers svg target view rot
               in do addEventListener mousemove drag false target
                     addEventListener mouseleave drop false target
                     addEventListener mouseup drop false target
                     removeEventListener mousedown down false target
  in addEventListener mousedown down false target

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

svgns :: Nullable String
svgns = toNullable (Just "http://www.w3.org/2000/svg")

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = void $ logErrors mainM

logErrors :: forall e a.  Eff (err :: EXCEPTION, console :: CONSOLE | e) a
          -> Eff (console :: CONSOLE | e) (Maybe a)
logErrors m = catchException handle (Just <$> m)
    where handle err = do log $ message err
                          return Nothing
