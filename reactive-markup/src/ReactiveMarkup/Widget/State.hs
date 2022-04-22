module ReactiveMarkup.Widget.State where

import Data.RHKT
import GHC.Generics
import ReactiveMarkup.Markup (Dynamic, DynamicF, Markup, Render, markup)
import ReactiveMarkup.Update
import Optics.Core

data LocalUpdate s e = LocalUpdate {localModel :: s, propagatedEvent :: Maybe e}
  deriving (Generic)

data LocalState s t c e = forall innerE. ZipTraverseF s => LocalState (innerE -> LocalUpdate (s Update) e -> LocalUpdate (s Update) e) (s ID) (s (DynamicF t) -> Markup t c innerE)

localState ::
  (ZipTraverseF s, Render (LocalState s t c) t c) =>
  (innerEvent -> LocalUpdate (s Update) outerEvent -> LocalUpdate (s Update) outerEvent) ->
  s ID ->
  (s (DynamicF t) -> Markup t c innerEvent) ->
  Markup t c outerEvent
localState f s m = markup $ LocalState f s m


simpleLocalState :: forall s t c innerEvent outerEvent. (ZipTraverseF (Wrap (Direct s)), Render (LocalState (Wrap (Direct s)) t c) t c) =>
  (innerEvent -> LocalUpdate s outerEvent -> LocalUpdate s outerEvent) ->
  s ->
  (Dynamic t s -> Markup t c innerEvent) ->
  Markup t c outerEvent
simpleLocalState f s makeMarkup = markup $ LocalState f' (Wrap $ ID s) makeMarkup'
  where 
    f' e (LocalUpdate m _) = 
      let s = m ^. (gfield @"wrapped" % deeper)
          LocalUpdate s' outerEvent =  f e (LocalUpdate s Nothing)
      in LocalUpdate (m & (gfield @"wrapped" % deeper) .~ s') outerEvent
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