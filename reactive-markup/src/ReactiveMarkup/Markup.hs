{-# LANGUAGE AllowAmbiguousTypes #-}

module ReactiveMarkup.Markup where

import Prelude hiding ((.), id)
import Control.Category
import Data.Text
import Data.Void

class Render widget target context where
  render :: widget e -> RenderTarget target context e

type family RenderTarget target context :: * -> *

data family Dynamic target a :: *

data Markup target context e = forall widget. Render widget target context => Markup (widget e)

markup :: (Render widget target context) => widget e -> Markup target context e
markup = Markup

renderMarkup :: forall target context e. Markup target context e -> RenderTarget target context e
renderMarkup (Markup elem) = render @_ @target @context @e elem

data Combine t c e = Combine (Markup t c e) (Markup t c e)

combine :: Render (Combine t c) t c => Markup t c e -> Markup t c e -> Markup t c e
combine a b = markup $ Combine a b

instance Render (Combine t c) t c => Semigroup (Markup t c e) where
  (<>) = combine

data Empty e = Empty

instance (Semigroup (Markup t c e), Render Empty t c) => Monoid (Markup t c e) where
  mempty = markup Empty

data Map t c e = forall innerE. Map (innerE -> e) (Markup t c innerE)

mapEvent :: Render (Map t c) t c => (e1 -> e2) -> Markup t c e1 -> Markup t c e2
mapEvent f m = markup $ Map f m

instance (Render (Map t c) t c) => Functor (Markup t c) where
  fmap = mapEvent

data FilterEvents t c e = forall eI. FilterEvents (eI -> Maybe e) (Markup t c eI)

filterEvents :: Render (FilterEvents t c) t c => (eI -> Maybe e) -> Markup t c eI -> Markup t c e
filterEvents f m = markup $ FilterEvents f m

dropEvents :: Render (FilterEvents t c) t c => Markup t c e -> Markup t c Void
dropEvents = filterEvents $ const Nothing

-- Optional Parameters
-- oMarkup :: w e -> Markup t c e
-- oMarkup :: w e -> (w e -> w e) -> Markup t c e

type Optional a b = forall r. (OptionalClass a b r, Out r ~ b, Result a b r ~ r) => r

oMarkup :: forall w t c e. (Render w t c) => w e -> Optional (w e) (Markup t c e)
oMarkup = makeOptional (markup :: w e -> Markup t c e)

type family Out r where
  Out (a -> b) = b
  Out b = b

type family Result a b r where
  Result a b b = b
  Result a b x = (a -> a) -> b

class OptionalClass a b r where
  makeOptional :: (a -> b) -> a -> r

instance OptionalClass a r ((a -> a) -> r) where
  makeOptional m a = \f -> m (f a)

instance OptionalClass a b b where
  makeOptional m a = m a
