import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Void
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (Dynamic)
import ReactiveMarkup
import ReactiveMarkup.Target.Gtk
import ReactiveMarkup.Widget.Diagram hiding (Diagram)
import Text.Read

newtype Model = Model Int

data AppEvent = Increase | Decrease | Set Int

valueApp :: Markup Gtk Root Void
valueApp =
  simpleLocalState updateModel (Model 0) $ \model ->
    column
      [buttons, chart model, textfield model]

updateModel :: AppEvent -> Model -> Maybe Model
updateModel event (Model i) = Just $
  Model $
    validate $ case event of
      Increase -> i + 1
      Decrease -> i - 1
      Set n -> n
  where
    validate x = min 10 $ max 0 x

buttons :: Markup Gtk Block AppEvent
buttons =
  row $
    fmap (margin Small) $
      [ button [\bO -> bO {click = Just Increase}] "Increase",
        button [\bO -> bO {click = Just Decrease}] "Decrease"
      ]

chart :: Dynamic Gtk Model -> Markup Gtk Block AppEvent
chart model = margin Big $ diagram $ fmap modelToDiagram model
  where
    modelToDiagram :: Model -> Diagram Cairo
    modelToDiagram (Model i) =
      rect 2 (fromIntegral (10 - i))
        === rect 2 (fromIntegral i) # fillColor blue

textfield :: Dynamic Gtk Model -> Markup Gtk Block AppEvent
textfield model =
  row $
    fmap (margin Small) $
      [ "Value: ",
        filterEvents id $
          let modelAsText = fmap (\(Model i) -> T.pack (show i)) model
           in textField [\tO -> tO {change = Just parse}] modelAsText
      ]
  where
    parse :: T.Text -> Maybe AppEvent
    parse = fmap Set . readMaybe @Int . T.unpack

main :: IO ()
main = do
  runGtk app

app :: App Gtk EmptyF Void
app =
  App
    { appRender = \_ -> valueApp,
      appHandleEvent = absurd,
      appInitialState = EmptyF,
      appName = "Value App"
    }