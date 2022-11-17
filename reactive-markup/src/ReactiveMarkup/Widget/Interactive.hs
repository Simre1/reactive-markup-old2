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
    wrapMarkup,
  )

data Button t c e = Button
  { click :: Maybe e,
    child :: Markup t c Void
  }
  deriving (Generic)



button :: forall t c e r. (Render (Button t Paragraph) t c) => Markup t Paragraph Void -> Markup t c e
button child =
  wrapMarkup $
    Button
      { click = Nothing,
        child = child
      }

data TextField t e = TextField
  { activate :: Maybe (Text -> e),
    change :: Maybe (Text -> e),
    text :: Dynamic t Text
  }
  deriving (Generic)

textField :: forall t c e. (Render (TextField t) t c) => Dynamic t Text -> Markup t c e
textField = wrapMarkup . TextField Nothing Nothing

data MapEventIO t c e = forall innerE. MapEventIO (innerE -> IO (Maybe e)) (Markup t c innerE)

mapEventIO :: Render (MapEventIO t c) t c => (innerE -> IO (Maybe e)) -> Markup t c innerE -> Markup t c e
mapEventIO f m = wrapMarkup $ MapEventIO f m

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
hotKeyFunction f child = wrapMarkup $ HotKey f child

hotKey :: Render (HotKey t c) t c => Key -> [Modifier] -> e -> Markup t c e -> Markup t c e
hotKey givenKey givenModifiers event = hotKeyFunction $ \key modifiers ->
  if (givenKey == key) && all (`elem` modifiers) givenModifiers
    then Just event
    else Nothing
