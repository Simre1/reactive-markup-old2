{-# LANGUAGE AllowAmbiguousTypes #-}

module ReactiveMarkup.Markup where

import Data.Kind (Constraint)
import Data.RHKT (FunctorF)
import Data.Void (Void)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Data.Foldable

class Render widget backend context where
  render :: widget e -> RenderTarget backend context e

type family RenderError widget backend context where
  RenderError widget backend context =
    TypeError
      ( Text "The widget \"" :<>: ShowType widget :<>: Text "\" cannot be rendered to \"Markup "
          :<>: ShowType backend
          :<>: Text " "
          :<>: ShowType context
          :<>: Text "\""
          :$$: Text "Most likely you need to use \""
          :<>: ShowType widget
          :<>: Text "\" in another context!"
      )

type family RenderErrorOnEqual a b widget backend context :: Constraint where
  RenderErrorOnEqual a a _ _ _ = ()
  RenderErrorOnEqual _ _ widget backend context = RenderError widget backend context

-- instance {-# OVERLAPPABLE #-} RenderError widget backend context =>
--   Render widget backend context where
--   render = error "no render"

type family RenderTarget backend context :: * -> *

data family Dynamic backend a :: *

type DynamicF backend = FunctorF (Dynamic backend)

data Markup backend context e = forall widget. Render widget backend context => Markup (widget e)

markup :: (Render widget backend context) => widget e -> Markup backend context e
markup = Markup

renderMarkup :: forall backend context e. Markup backend context e -> RenderTarget backend context e
renderMarkup (Markup elem) = render @_ @backend @context @e elem

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

newtype Lift t c e = Lift (Markup t c e)

lift :: forall c1 c2 t e. Render (Lift t c1) t c2 => Markup t c1 e -> Markup t c2 e
lift = markup . Lift

-- Optional Parameters
-- oMarkup :: (o -> w e) -> Markup t c e
-- oMarkup :: (o -> w e) -> (o -> o) -> Markup t c e

withDefaultParameter :: (d -> x) -> d -> [d -> d] -> x
withDefaultParameter f d c = f $ foldl' (.) id c d

-- type Optional a b r = (OptionalClass a b r, CalcR a b r ~ r, CalcB a r ~ b)

-- oMarkup :: forall w t c o e r. (Render w t c, Optional o (Markup t c e) r) => (o -> w e) -> o -> r
-- oMarkup f o = makeOptional (markup . f :: o -> Markup t c e) o

-- oMarkup1 :: forall w t c o e r x. (Render w t c, Optional o (x -> Markup t c e) r) => (o -> x -> w e) -> o -> CalcR o (x -> Markup t c e) r
-- oMarkup1 f o = makeOptional (fmap markup . f :: o -> x -> Markup t c e) o

-- oMarkup2 :: forall w t c o e r x y. (Render w t c, Optional o (x -> y -> Markup t c e) r) => (o -> x -> y -> w e) -> o -> r
-- oMarkup2 f o = makeOptional (fmap (fmap markup) . f :: o -> x -> y -> Markup t c e) o

-- type family CalcB a r where
--   CalcB a ([a -> a] -> b) = b
--   CalcB _ b = b

-- type family CalcR a b r where
--   CalcR a b b = b
--   CalcR a b _ = [a -> a] -> b

-- class OptionalClass a b r where
--   makeOptional :: (a -> b) -> a -> r

-- instance OptionalClass a b ([a -> a] -> b) where
--   makeOptional m a = \f -> m (foldl (.) id f $ a)

-- instance OptionalClass a b b where
--   makeOptional m a = m a
