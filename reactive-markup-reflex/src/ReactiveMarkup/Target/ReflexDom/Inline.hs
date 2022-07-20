module ReactiveMarkup.Target.ReflexDom.Inline where

import Data.Functor (void)
import ReactiveMarkup
import ReactiveMarkup.Target.ReflexDom.Base
import Reflex.Dom.Widget as W

instance Render Words RDom c where
  render (Words t) = ReflexWidget (\_ -> W.text t)

instance Render (Bold RDom Inline) RDom c where
  render (Bold m) = ReflexWidget $ \t -> void . W.el' "b" $ renderReflexWidget t m

instance Render (Italic RDom Inline) RDom c where
  render (Italic m) = ReflexWidget $ \t -> void . W.el' "i" $ renderReflexWidget t m

instance Render (Combine RDom Inline) RDom c where
  render (Combine m1 m2) = ReflexWidget $ \t -> renderReflexWidget t m1 >> renderReflexWidget t m2
