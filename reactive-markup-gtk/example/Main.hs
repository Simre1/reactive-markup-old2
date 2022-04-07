module Main where

import Data.Default
import Data.Text as T
import qualified Data.Text.Read as T
import Data.Void
import Optics.Core
import Optics.Generic
import ReactiveMarkup.App
import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk
import ReactiveMarkup.Widgets.Base
import ReactiveMarkup.Widgets.Eventful
import Data.Functor

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

newtype TempModel = Celsius Int deriving (Show)

fahreinheit :: TempModel -> Int
fahreinheit (Celsius n) = (n * 9 `quot` 5) + 32

celsius :: TempModel -> Int
celsius (Celsius n) = n

setFahreinheit :: Int -> TempModel -> TempModel
setFahreinheit n _ = Celsius $ (n - 32) * 5 `quot` 9

setCelsius :: Int -> TempModel -> TempModel
setCelsius n _ = Celsius n

data AppEvent = SetCelsius Int | SetFahreinheit Int | Search Text

handleEvent :: AppEvent -> TempModel -> IO TempModel
handleEvent (SetFahreinheit n) m = pure $ setFahreinheit n m
handleEvent (SetCelsius n) m = pure $ setCelsius n m
handleEvent (Search t) m = print t $> m

renderGUI :: Dynamic Gtk TempModel -> Markup Gtk Root AppEvent
renderGUI model =
  column
    [ "Celsius",
      (fmap :: (Int -> AppEvent) -> Markup Gtk Block Int -> Markup Gtk Block AppEvent) SetCelsius $
        numberField
          ((fmap :: (TempModel -> Int) -> Dynamic Gtk TempModel -> Dynamic Gtk Int) celsius model),
      "Fahreinheit",
      SetFahreinheit <$> numberField (fahreinheit <$> model),
      searchComponent
    ]

app :: App TempModel Gtk AppEvent
app =
  App
    { appRender = renderGUI,
      appHandleEvent = handleEvent,
      appInitialState = Celsius 0
    }

numberField :: Dynamic Gtk Int -> Markup Gtk Block Int
numberField state =
  let text :: Dynamic Gtk Text = pack . show <$> state
   in filterEvents parseNumber $ textField $ (#value .~ text) . (#change ?~ id)
  where
    parseNumber :: TextFieldEvent -> Maybe Int
    parseNumber (TextFieldEvent t) =
      either
        (const Nothing)
        ( \(num, t) ->
            if T.null t then Just num else Nothing
        )
        $ T.signed T.decimal t


-- data AppEvent = Search Text

data SearchEvent = SearchButtonClicked | UpdateSearchText Text

searchComponent :: Markup Gtk Block AppEvent
searchComponent = localState handleSearchEvent "" searchWithButton
  where
    handleSearchEvent :: Text -> SearchEvent -> (Maybe Text, Maybe AppEvent)
    handleSearchEvent searchText SearchButtonClicked = (Nothing, Just (Search searchText))
    handleSearchEvent _ (UpdateSearchText t) = (Just t, Nothing)
    
    searchWithButton :: Dynamic Gtk Text -> Markup Gtk Block SearchEvent
    searchWithButton searchText = 
      row
        [ textField $ (#value .~ searchText) . (#change ?~ (\(TextFieldEvent t) -> UpdateSearchText t)),
          SearchButtonClicked <$ button "Search"
        ]

