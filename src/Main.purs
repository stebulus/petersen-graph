module Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Array (nubBy, zipWith)
import Data.Foldable (intercalate, mconcat)
import Data.Functor.Contravariant (cmap)
import Data.List.Lazy (fromList, iterate, take)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Monoid (Monoid)
import Data.Traversable (for)
import Data.Tuple (fst, snd, Tuple(Tuple))
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
import Phi
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
  setAttribute "r" (show (sqrt 3.0)) world
  view <- mconcat <$> for petersenEdges \(Edge u v) -> do
    elem <- createElementNS svgns "line" (htmlDocumentToDocument doc)
    appendChild (elementToNode elem) (elementToNode g)
    return (rotatedLine elem (fromPhiInt <$> u) (fromPhiInt <$> v))
  runView view initialRotation
  installDragHandlers svg world view initialRotation
  dragme <- mustGetElementById (ElementId "drag-me") doc'
  let hideDragMe = eventListener \evt -> do
        setAttribute "class" "dragged" dragme
        removeEventListener mousedown hideDragMe false (elementToEventTarget world)
  addEventListener mousedown hideDragMe false (elementToEventTarget world)
  setTextContent "drag me" (elementToNode dragme)

data Edge a = Edge (Vector a) (Vector a)

-- | Inscribe a regular dodecahedron in a sphere, then identify
-- | opposite points (turning the sphere into the projective plane);
-- | the edge graph of the dodecahedron becomes the Petersen graph.

petersenEdges :: forall a. (Eq a, Ring a) => Array (Edge (Phi a))
petersenEdges = nubBy edgeEquiv dodecahedronEdges
  where edgeEquiv (Edge u1 u2) (Edge v1 v2) = equiv u1 v1 && equiv u2 v2
        equiv u v = u == v || u == (negate <$> v)

-- | Construct a regular dodecahedron circumscribed around a cube,
-- | following Euclid's method of adding a small tent-shaped polyhedron
-- | to each face of the cube.  Each vertex of the cube is also a
-- | vertex of the dodecahedron; each tent contributes two additional
-- | vertices.  Each tent vertex is joined to its tent sibling and
-- | to two of the cube's vertices.
-- | http://aleph0.clarku.edu/~djoyce/elements/bookXIII/propXIII17.html

dodecahedronEdges :: forall a. (Ring a) => Array (Edge (Phi a))
dodecahedronEdges = cornerEdges ++ faceEdges
  where cornerEdges = do mask <- vector <$> bools <*> bools <*> bools
                         let corner = negateSome mask baseCorner
                         face <- cycles <*> pure baseFace
                         return (Edge corner (negateSome mask face))
        faceEdges = do bool <- bools
                       let v1 = negateSome (vector false false bool) baseFace
                       let v2 = negateSome (vector false true bool) baseFace
                       cycle <- cycles
                       return (Edge (cycle v1) (cycle v2))
        baseCorner = vector one one one  -- cube vertex
        baseFace = vector zero (phi - one) phi  -- tent vertex
        cycle1 (Vector v) = vector v.y v.z v.x
        cycles :: Array (Vector (Phi a) -> Vector (Phi a))
        cycles = [id, cycle1, cycle1 <<< cycle1]
        bools = [true, false]
        negateSome mask v = negateIf <$> mask <*> v
        negateIf flag x = if flag then negate x else x

-- | Conversion between 3d coordinates and screen coordinates.
-- | Stereographically project the `z >= 0` half of R^3 to via the
-- | south pole to the `z = 0` plane.  Likewise project the `z <= 0`
-- | half via the north pole, but also negate the resulting point.
-- | Net effect: The unit circle models the projective plane.

toScreen :: Vector Number -> Screen
toScreen (Vector pt) = { u: t * pt.x
                       , v: t * pt.y
                       }
  where t = 1.0/(pt.z + if pt.z >= 0.0 then 1.0 else -1.0)

fromScreen :: Screen -> UnitVector Number
fromScreen s = normalize $ vector (2.0*s.u)
                                  (2.0*s.v)
                                  (1.0 - s.u*s.u - s.v*s.v)

rotatedLine :: forall e. Element -> Vector Number -> Vector Number
            -> View (dom :: DOM | e) (UnitQuaternion Number)
rotatedLine elem p q =
  cmap (\u -> let f = toScreen <<< rotate u in Tuple (f p) (f q))
       (lineView elem)

lineView :: forall e. Element -> View (dom :: DOM | e) (Tuple Screen Screen)
lineView elem = mconcat [ f "x1" \t -> (fst t).u
                        , f "y1" \t -> (fst t).v
                        , f "x2" \t -> (snd t).u
                        , f "y2" \t -> (snd t).v
                        ]
  where f attr get = cmap (show <<< get) (attributeView elem attr)

-- | Install event handlers to rotate the figure in response to
-- | dragging, as follows.  Convert the start and end of the drag
-- | (screen points) to points on the unit sphere; rotate on an axis
-- | perpendicular to the great circle between those two points,
-- | by an angle sufficient to move the start to the end.

installDragHandlers :: forall e. Element -> Element
                    -> View (dom :: DOM | e) (UnitQuaternion Number)
                    -> UnitQuaternion Number
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

-- | Initial rotation of the figure: a face of the dodecahedron
-- | centred, and the resulting star pointing up.

initialRotation :: UnitQuaternion Number
initialRotation = ninety <> rotater unitZ centre
  where centre = normalize (fromPhiInt <$> mconcat face)
        face = [ vector one one one
               , vector zero (phi - one) phi
               , vector zero (one - phi) phi
               , vector one (negate one) one
               , vector phi zero (phi - one)
               ]
        ninety = axisAngle unitZ (Radians (pi/2.0))
