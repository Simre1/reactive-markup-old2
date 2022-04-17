{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}




module ReactiveMarkup.Markup where

import Data.RHKT (FunctorF)
import Data.Void (Void)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Kind (Constraint)

class Render widget target context where
  render :: widget e -> RenderTarget target context e

type family RenderError widget target context where
   RenderError widget target context = TypeError 
      (Text "The widget " :<>: ShowType widget :<>: Text " cannot occur in " :<>: 
      ShowType context :<>: Text " when rendering to " :<>: ShowType target :<>: Text ".") 

type family RenderErrorOnEqual a b widget target context:: Constraint where
  RenderErrorOnEqual a a _ _ _ = ()
  RenderErrorOnEqual _ _ widget target context = RenderError widget target context


instance {-# OVERLAPPABLE #-} RenderError widget target context => Render widget target context

type family RenderTarget target context :: * -> *

data family Dynamic target a :: *

type DynamicF target = FunctorF (Dynamic target)

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

newtype Lift t c e = Lift (Markup t c e)

lift :: forall c1 c2 t e. Render (Lift t c1) t c2 => Markup t c1 e -> Markup t c2 e
lift = markup . Lift

-- Optional Parameters
-- oMarkup :: (o -> w e) -> Markup t c e
-- oMarkup :: (o -> w e) -> (o -> o) -> Markup t c e

type Optional a b = forall r. (OptionalClass a b r, Out r ~ b, Result a b r ~ r) => r

oMarkup :: forall w t c o e. (Render w t c) => (o -> w e) -> o -> Optional o (Markup t c e)
oMarkup f o = makeOptional (markup . f :: o -> Markup t c e) o

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
