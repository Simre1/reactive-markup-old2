module ReactiveMarkup.Widget.State where

import ReactiveMarkup.Markup (Dynamic, Markup, Render, markup)

data LocalState s t c e = forall innerE. LocalState (s -> innerE -> (Maybe s, Maybe e)) s (Dynamic t s -> Markup t c innerE)

localState ::
  Render (LocalState s t c) t c =>
  (s -> innerEvent -> (Maybe s, Maybe outerEvent)) ->
  s ->
  (Dynamic t s -> Markup t c innerEvent) ->
  Markup t c outerEvent
localState f s m = markup $ LocalState f s m

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