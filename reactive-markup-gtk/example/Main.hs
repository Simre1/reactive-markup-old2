module Main where

import Data.Text as T
import qualified Data.Text.Read as T
import Data.Void
import Optics.Core
import Optics.Generic
import ReactiveMarkup
import ReactiveMarkup.Target.Gtk
import Data.Functor
import GHC.Generics
import Control.Monad.IO.Class

{-

   ---- TempModel+Event
   |     ᐱ       ᐱ
   |     |       |
   |     |     Event
   |     |       ᐱ
   |     |       |
   --> TempModel -> GUI
-}

main :: IO ()
main = do
  runGtk app

newtype TempModel f = TempModel {
  modelCelsius :: f (Direct Int)
  } deriving Generic

instance TransformFData TempModel where
  transformFData fD fN (TempModel a) (TempModel b) = TempModel <$> fD a b

fahreinheit :: TempModel (DynamicF Gtk) -> Dynamic Gtk Int
fahreinheit model = (\n -> ((n * 9) `quot` 5) + 32) <$> celsius model

celsius :: TempModel (DynamicF Gtk) -> Dynamic Gtk Int
celsius model = model ^. #modelCelsius % deeper

setFahreinheit :: Monad m => Int -> ModelM TempModel m ()
setFahreinheit n = setCelsius $ ((n - 32) * 5) `quot` 9

setCelsius :: Monad m => Int -> ModelM TempModel m ()
setCelsius n = mPut #modelCelsius n

data AppEvent = SetCelsius Int | SetFahreinheit Int | Search Text

handleEvent :: AppEvent -> ModelM TempModel IO ()
handleEvent (SetFahreinheit n) = setFahreinheit n
handleEvent (SetCelsius n) = setCelsius n
handleEvent (Search t) = liftIO $ print t

renderGUI :: TempModel (DynamicF Gtk) -> Markup Gtk Root AppEvent
renderGUI model =
  column
    [ "Celsius",
      (fmap :: (Int -> AppEvent) -> Markup Gtk Block Int -> Markup Gtk Block AppEvent) SetCelsius $
        numberField (celsius model),
      "Fahreinheit",
      SetFahreinheit <$> numberField (fahreinheit model),
      searchComponent,
      absurd <$> countingButton
    ]

app :: App Gtk TempModel AppEvent
app =
  App
    { appRender = renderGUI,
      appHandleEvent = handleEvent,
      appInitialState = TempModel $ ID 0,
      appName = "Temperature Example"
    }

numberField :: Dynamic Gtk Int -> Markup Gtk Block Int
numberField state = 
  let text :: Dynamic Gtk Text = pack . show <$> state
   in filterEvents parseNumber $ textField [(#change ?~ id)] (text)
  where
    parseNumber :: Text -> Maybe Int
    parseNumber t =
      either
        (const Nothing)
        ( \(num, t) ->
            if T.null t then Just num else Nothing
        )
        $ T.signed T.decimal t


-- data AppEvent = Search Text

data SearchEvent = SearchButtonClicked | UpdateSearchText Text

searchComponent :: Markup Gtk Block AppEvent
searchComponent = simpleLocalState' handleSearchEvent "" searchWithButton
  where
    handleSearchEvent :: SearchEvent -> Text -> SimpleUpdate Text AppEvent
    handleSearchEvent SearchButtonClicked s = setSimpleUpdateEvent (Search s) defSimpleUpdate
    handleSearchEvent (UpdateSearchText t) _ = setSimpleUpdate t defSimpleUpdate
    
    searchWithButton :: Dynamic Gtk Text -> Markup Gtk Block SearchEvent
    searchWithButton searchText = 
      row
        [ textField [(#change ?~ UpdateSearchText)] (searchText),
          button [#click ?~ SearchButtonClicked] "Search"
        ]

countingButton :: Markup Gtk Block Void
countingButton = simpleLocalState handleButtonClick initialState buttonWithNumber
  where
    initialState :: Int
    initialState = 0

    handleButtonClick :: () -> Int -> Maybe Int
    handleButtonClick () state = Just (state + 1)
    
    buttonWithNumber :: Dynamic Gtk Int -> Markup Gtk Block ()
    buttonWithNumber int = dynamicMarkup int $ \i -> button [(#click ?~ ())] (string $ show i)
