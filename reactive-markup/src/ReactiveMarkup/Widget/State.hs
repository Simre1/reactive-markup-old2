module ReactiveMarkup.Widget.State where

import Data.Functor.Identity
import Data.RHKT
import GHC.Generics
import Optics.Core
import ReactiveMarkup.Markup (Dynamic, DynamicF, Markup, Render, markup)
import ReactiveMarkup.Update

data LocalUpdate s e = LocalUpdate {localModel :: s, propagatedEvent :: Maybe e}
  deriving (Generic)

data LocalState s t c e = forall innerE. TransformFData s => LocalState (innerE -> ModelM s Identity (Maybe e)) (s ID) (s (DynamicF t) -> Markup t c innerE)

localState ::
  (TransformFData s, Render (LocalState s t c) t c) =>
  (innerEvent -> ModelM s Identity (Maybe outerEvent)) ->
  s ID ->
  (s (DynamicF t) -> Markup t c innerEvent) ->
  Markup t c outerEvent
localState f s m = markup $ LocalState f s m

data SimpleUpdate s e = SimpleUpdate (Maybe s) (Maybe e)

defSimpleUpdate :: SimpleUpdate s e
defSimpleUpdate = SimpleUpdate Nothing Nothing

setSimpleUpdate :: s -> SimpleUpdate s e -> SimpleUpdate s e
setSimpleUpdate s (SimpleUpdate _ e) = SimpleUpdate (Just s) e

setSimpleUpdateEvent :: e -> SimpleUpdate s e -> SimpleUpdate s e
setSimpleUpdateEvent e (SimpleUpdate s _) = SimpleUpdate s (Just e)

simpleLocalState' ::
  forall s t c innerEvent outerEvent.
  (TransformFData (Wrap (Direct s)), Render (LocalState (Wrap (Direct s)) t c) t c) =>
  (innerEvent -> s -> SimpleUpdate s outerEvent) ->
  s ->
  (Dynamic t s -> Markup t c innerEvent) ->
  Markup t c outerEvent
simpleLocalState' f s makeMarkup = markup $ LocalState f' (Wrap $ ID s) makeMarkup'
  where
    f' :: innerEvent -> ModelM (Wrap (Direct s)) Identity (Maybe outerEvent)
    f' e = do
      s <- mGet (gfield @"wrap")
      let SimpleUpdate s' outerEvent = f e s
      maybe (pure ()) (mPut (gfield @"wrap")) s'
      pure outerEvent
    makeMarkup' :: Wrap (Direct s) (DynamicF t) -> Markup t c innerEvent
    makeMarkup' (Wrap (FunctorF s)) = makeMarkup s

simpleLocalState ::
  forall s t c innerEvent outerEvent.
  (ZipTraverseF (Wrap (Direct s)), Render (LocalState (Wrap (Direct s)) t c) t c) =>
  (innerEvent -> s -> Maybe s) ->
  s ->
  (Dynamic t s -> Markup t c innerEvent) ->
  Markup t c outerEvent
simpleLocalState f s makeMarkup = markup $ LocalState f' (Wrap $ ID s) makeMarkup'
  where
    f' :: innerEvent -> ModelM (Wrap (Direct s)) Identity (Maybe outerEvent)
    f' e = do
      s <- mGet (gfield @"wrap")
      let s' = f e s
      maybe (pure ()) (mPut (gfield @"wrap")) s'
      pure Nothing
    makeMarkup' :: Wrap (Direct s) (DynamicF t) -> Markup t c innerEvent
    makeMarkup' (Wrap (FunctorF s)) = makeMarkup s

data DynamicMarkup s t c e = DynamicMarkup (Dynamic t s) (s -> Markup t c e)

dynamicMarkup :: Render (DynamicMarkup s t c) t c => Dynamic t s -> (s -> Markup t c e) -> Markup t c e
dynamicMarkup d f = markup $ DynamicMarkup d f

data LocalStateIO t c e = forall s innerE. LocalStateIO (s -> innerE -> IO (s, Maybe e)) s (s -> Markup t c innerE)

localStateIO ::
  Render (LocalStateIO t c) t c =>
  (s -> innerEvent -> IO (s, Maybe e)) ->
  s ->
  (s -> Markup t c innerEvent) ->
  Markup t c e
localStateIO f s m = markup $ LocalStateIO f s m

data Counter t c e = Counter Double (Dynamic t Int -> Markup t c e)

counter :: Render (Counter t c) t c => Double -> (Dynamic t Int -> Markup t c e) -> Markup t c e
counter i = markup . Counter i