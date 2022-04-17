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
    oMarkup,
  )

newtype ButtonOptions e = ButtonOptions
  { click :: Maybe e
  }
  deriving (Generic)

data Button t c e = Button (Markup t c Void) (ButtonOptions e)

button ::
  (Render (Button t Inline) t c) =>
  Markup t Inline Void ->
  Optional (ButtonOptions e) (Markup t c e)
button i = oMarkup (Button i) (ButtonOptions Nothing)

data TextFieldOptions t e = TextFieldOptions
  { value :: Dynamic t Text,
    activate :: Maybe (Text -> e),
    change :: Maybe (Text -> e)
  }
  deriving (Generic)

newtype TextField t e = TextField (TextFieldOptions t e)

textField :: forall t c e. (Render (TextField t) t c, Applicative (Dynamic t)) => Optional (TextFieldOptions t e) (Markup t c e)
textField = oMarkup TextField $ TextFieldOptions @t (pure "") Nothing Nothing

data MapEventIO t c e = forall innerE. MapEventIO (innerE -> IO (Maybe e)) (Markup t c innerE)

mapEventIO :: Render (MapEventIO t c) t c => (innerE -> IO (Maybe e)) -> Markup t c innerE -> Markup t c e
mapEventIO f m = markup $ MapEventIO f m
