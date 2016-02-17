module DOMExtra where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (error, EXCEPTION(), throwException)
import Data.Array (filter)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Nullable (Nullable(), toMaybe, toNullable)
import Data.String (split)
import DOM (DOM())
import DOM.Node.Element (getAttribute, setAttribute)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element(), ElementId(ElementId), NonElementParentNode())
import Prelude

import View

svgns :: Nullable String
svgns = toNullable (Just "http://www.w3.org/2000/svg")

addClass :: forall e. String -> Element -> Eff (dom :: DOM | e) Unit
addClass cls elem = do
  mexistingcls <- toMaybe <$> getAttribute "class" elem
  let newcls = case mexistingcls of
                 Nothing -> cls
                 Just existingcls -> existingcls ++ " " ++ cls
  setAttribute "class" newcls elem

removeClass :: forall e. String -> Element -> Eff (dom :: DOM | e) Unit
removeClass cls elem = do
  mexistingcls <- toMaybe <$> getAttribute "class" elem
  case mexistingcls of
    Nothing -> return unit
    Just existingcls -> let newcls = split existingcls " "
                                     # filter (/= cls)
                                     # intercalate " "
                        in setAttribute "class" newcls elem

mustGetElementById :: forall e. ElementId -> NonElementParentNode
                   -> Eff (dom :: DOM, err :: EXCEPTION | e) Element
mustGetElementById elemid@(ElementId idstr) parent = do
  melem <- toMaybe <$> getElementById elemid parent
  case melem of
    Nothing -> throwException (error ("#" ++ idstr ++ " not found"))
    Just elem -> return elem

attributeView :: forall e. Element -> String -> View (dom :: DOM | e) String
attributeView elem attrname = View \attrvalue ->
  setAttribute attrname attrvalue elem
