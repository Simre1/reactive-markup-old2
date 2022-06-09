module ReactiveMarkup.Widget.Interactive where

import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import ReactiveMarkup.Context (Inline)
import ReactiveMarkup.Markup
  ( Dynamic,
    Markup,
    Optional,
    Render,
    markup,
    oMarkup, OptionalClass (makeOptional)
  )

newtype ButtonOptions e = ButtonOptions
  { click :: Maybe e
  }
  deriving (Generic)

data Button t c e = Button (ButtonOptions e) (Markup t c Void)

button :: (Render (Button t Inline) t c, Optional (ButtonOptions e) (Markup t Inline Void -> Markup t c e) r) => r
button = oMarkup Button (ButtonOptions Nothing)

test :: forall t c e. Render (Button t Inline) t c => Markup t c e
test = button id (undefined :: Markup t Inline Void)

data TextFieldOptions t e = TextFieldOptions
  { value :: Dynamic t Text,
    activate :: Maybe (Text -> e),
    change :: Maybe (Text -> e)
  }
  deriving (Generic)

-- newtype TextField t e = TextField (TextFieldOptions t e)

-- textField :: forall t c e. (Render (TextField t) t c, Applicative (Dynamic t)) => Optional (TextFieldOptions t e) (Markup t c e)
-- textField = oMarkup TextField $ TextFieldOptions @t (pure "") Nothing Nothing

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
