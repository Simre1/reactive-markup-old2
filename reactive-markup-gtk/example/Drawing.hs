import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Void
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (Dynamic)
import ReactiveMarkup
import ReactiveMarkup.Target.Gtk
import ReactiveMarkup.Widget.Diagram
import Text.Read

main :: IO ()
main = do
  runGtk app

newtype Model = Model Int

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
  row
    [ button [\bO -> bO {click = Just Increase}] "Increase",
      button [\bO -> bO {click = Just Decrease}] "Decrease"
    ]

chart :: Dynamic Gtk Model -> Markup Gtk Block AppEvent
chart model =
  diagram @Cairo $
    model <&> \(Model i) -> center $ 
      rect 1 (0.5 * fromIntegral (10 - i))
        === rect 1 (0.5 * fromIntegral i) # fillColor blue

textfield :: Dynamic Gtk Model -> Markup Gtk Block AppEvent
textfield model =
  row
    [ "Value: ",
      filterEvents id $
        textField [\tO -> tO {change = Just parse}] $ (\(Model i) -> T.pack (show i)) <$> model
    ]
  where
    parse :: T.Text -> Maybe AppEvent
    parse = fmap Set . readMaybe @Int . T.unpack

data AppEvent = Increase | Decrease | Set Int

app :: App Gtk EmptyF Void
app =
  App
    { appRender = \_ -> valueApp,
      appHandleEvent = absurd,
      appInitialState = EmptyF,
      appName = "Value App"
    }