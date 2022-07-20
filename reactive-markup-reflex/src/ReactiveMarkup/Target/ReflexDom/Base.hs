module ReactiveMarkup.Target.ReflexDom.Base where

import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Proxy (Proxy)
import ReactiveMarkup as M
import Reflex.Dom.Core as R
import Reflex.Host.Class
import qualified Reflex.Spider.Internal as I

data RDom

type Trigger a = a -> Performable (Widget DomTimeline) ()

type instance RenderTarget RDom c = ReflexWidget

newtype instance M.Dynamic RDom a = ReflexDomDynamic {getReflexDomDynamic :: R.Dynamic DomTimeline a}

instance Functor (M.Dynamic RDom) where
  fmap f (ReflexDomDynamic d) = ReflexDomDynamic (f <$> d)

instance Applicative (M.Dynamic RDom) where
  pure a = ReflexDomDynamic $ pure a
  (<*>) (ReflexDomDynamic d1) (ReflexDomDynamic d2) = ReflexDomDynamic $ d1 <*> d2

instance Monad (M.Dynamic RDom) where
  (ReflexDomDynamic d) >>= f = ReflexDomDynamic $ d >>= (getReflexDomDynamic . f)

newtype ReflexWidget e = ReflexWidget (Trigger e -> Widget DomTimeline ())

renderReflexWidget :: Trigger e -> Markup RDom c e -> Widget DomTimeline ()
renderReflexWidget t markup = let (ReflexWidget f) = renderMarkup markup in f t

reactimate :: Event DomTimeline a -> (a -> Performable (Widget DomTimeline) ()) -> Widget DomTimeline ()
reactimate ev f = performEvent_ $ f <$> ev

instance Render (Lift RDom Inline) RDom Block where
  render (Lift m) = ReflexWidget $ \t -> renderReflexWidget t m

instance Render (Lift RDom Inline) RDom Root where
  render (Lift m) = ReflexWidget $ \t -> renderReflexWidget t m

instance Render (Lift RDom Block) RDom Root where
  render (Lift m) = ReflexWidget $ \t -> renderReflexWidget t m