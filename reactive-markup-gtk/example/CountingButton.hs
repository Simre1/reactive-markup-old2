import Data.Void
import ReactiveMarkup
import ReactiveMarkup.Target.Gtk

data Model = Model Int

clickButton :: Markup Gtk Common ()
clickButton = button [\buttonOptions -> buttonOptions {click = Just ()}] (string "Click me")

countingButton :: Markup Gtk Common Void
countingButton =
  simpleLocalState
    initialState
    handleButtonClick
    buttonWithNumber
  where
    initialState :: Model
    initialState = Model 0

    handleButtonClick :: () -> Model -> Maybe Model
    handleButtonClick () (Model state) = Just (Model (state + 1))

    buttonWithNumber :: Dynamic Gtk Model -> Markup Gtk Common ()
    buttonWithNumber model =
      column
        [ dynamicMarkup model $ \(Model int) -> string $ show int,
          clickButton
        ]

app :: App Gtk EmptyF Void
app =
  App
    { appRender = \_ -> lift countingButton,
      appHandleEvent = absurd,
      appInitialState = EmptyF,
      appName = "Counting Button"
    }

main :: IO ()
main = runGtk app