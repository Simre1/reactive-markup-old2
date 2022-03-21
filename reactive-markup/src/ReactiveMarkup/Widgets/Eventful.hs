{-# LANGUAGE FunctionalDependencies #-}

module ReactiveMarkup.Widgets.Eventful where

import Data.Default
import Data.Text
import Data.Void
import GHC.Generics
import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Markup

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

data Button t c e = e ~ ButtonClick => Button (Markup t c Void)

data ButtonClick = ButtonClick

button :: Render (Button t Inline) t c => Markup t Inline Void -> Markup t c ButtonClick
button = markup . Button

newtype TextFieldEvent = TextFieldEvent Text

data TextField t e = TextField
  { value :: Dynamic t Text,
    activate :: Maybe (TextFieldEvent -> e),
    change :: Maybe (TextFieldEvent -> e)
  }
  deriving (Generic)

textField :: forall t c e. (Render (TextField t) t c, Applicative (Dynamic t)) => Optional (TextField t e) (Markup t c e)
textField = oMarkup $ TextField @t (pure "") Nothing Nothing

data NumberField t e = NumberField

