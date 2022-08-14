{-# LANGUAGE AllowAmbiguousTypes #-}

module ReactiveMarkup.Widget.Interactive where

import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import ReactiveMarkup.Context (Paragraph)
import ReactiveMarkup.Markup
  ( Dynamic,
    Markup,
    Render,
    markup,
    withDefaultParameter,
  )

newtype ButtonOptions e = ButtonOptions
  { click :: Maybe e
  }
  deriving (Generic)

data Button t c e = Button (ButtonOptions e) (Markup t c Void)

button :: forall t c e r. (Render (Button t Paragraph) t c) => [ButtonOptions e -> ButtonOptions e] -> Markup t Paragraph Void -> Markup t c e
button = withDefaultParameter (fmap markup . Button) (ButtonOptions Nothing)

data TextFieldOptions t e = TextFieldOptions
  { activate :: Maybe (Text -> e),
    change :: Maybe (Text -> e)
  }
  deriving (Generic)

data TextField t e = TextField (TextFieldOptions t e) (Dynamic t Text) 

textField :: forall t c e. (Render (TextField t) t c) => [TextFieldOptions t e -> TextFieldOptions t e] -> Dynamic t Text -> Markup t c e
textField = withDefaultParameter (fmap markup . TextField) $ TextFieldOptions Nothing Nothing

data MapEventIO t c e = forall innerE. MapEventIO (innerE -> IO (Maybe e)) (Markup t c innerE)

mapEventIO :: Render (MapEventIO t c) t c => (innerE -> IO (Maybe e)) -> Markup t c innerE -> Markup t c e
mapEventIO f m = markup $ MapEventIO f m

data HotKey t c e = HotKey (Key -> [Modifier] -> Maybe e) (Markup t c e)

data Modifier = ModShift | ModControl | ModAlt | ModSuper deriving (Eq, Show)

data Key
  = KeyQ
  | KeyW
  | KeyE
  | KeyR
  | KeyT
  | KeyZ
  | KeyU
  | KeyI
  | KeyO
  | KeyP
  | KeyA
  | KeyS
  | KeyD
  | KeyF
  | KeyJ
  | KeyK
  | KeyL
  | KeyY
  | KeyX
  | KeyC
  | KeyV
  | KeyB
  | KeyN
  | KeyM
  | KeyEnter
  | KeySpace
  | KeyDown
  | KeyUp
  | KeyLeft
  | KeyRight
  deriving (Eq, Show)

hotKeyFunction :: Render (HotKey t c) t c => (Key -> [Modifier] -> Maybe e) -> Markup t c e -> Markup t c e
hotKeyFunction f child = markup $ HotKey f child

hotKey :: Render (HotKey t c) t c => Key -> [Modifier] -> e -> Markup t c e -> Markup t c e
hotKey givenKey givenModifiers event = hotKeyFunction $ \key modifiers ->
  if (givenKey == key) && all (`elem` modifiers) givenModifiers
    then Just event
    else Nothing
