module ReactiveMarkup.Target.ReflexDom.Container where

import Data.Foldable
import Data.Functor (void)
import ReactiveMarkup
import ReactiveMarkup.Target.ReflexDom.Base
import Reflex.Dom ((=:))
import Reflex.Dom.Widget as W

instance Render (Column RDom Block) RDom Block where
  render (Column ms) = ReflexWidget $ \t ->
    W.elAttr "div" ("style" =: "display: flex; flex-direction: column;") $ traverse_ (renderReflexWidget t) ms

instance Render (Row RDom Block) RDom Block where
  render (Row ms) = ReflexWidget $ \t ->
    W.elAttr "div" ("style" =: "display: flex; flex-direction: row;") $ traverse_ (renderReflexWidget t) ms