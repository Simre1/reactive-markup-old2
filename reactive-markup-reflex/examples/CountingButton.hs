import Data.Void
import ReactiveMarkup
import ReactiveMarkup.Target.ReflexDom

data Model = Model Int

clickButton :: Markup RDom Common ()
clickButton = button (string "Click me") $= \button -> button {click = Just ()}

countingButton :: Markup RDom Common Void
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

    buttonWithNumber :: Dynamic RDom Model -> Markup RDom Common ()
    buttonWithNumber model =
      column
        [ dynamicMarkup model $ \(Model int) -> (string $ show int),
          clickButton
        ]

app :: App RDom EmptyF Void
app =
  App
    { appRender = \_ -> lift countingButton,
      appHandleEvent = absurd,
      appInitialState = EmptyF,
      appName = "Counting Button"
    }

main :: IO ()
main = runReflexDomWarp app