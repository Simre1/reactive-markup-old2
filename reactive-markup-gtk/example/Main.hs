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
import Data.RHKT

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
  }

instance ZipTraverseF TempModel where
  zipTraverseF fD fN (TempModel a) (TempModel b) = TempModel <$> fD a b

fahreinheit :: TempModel (DynamicF Gtk) -> Dynamic Gtk Int
fahreinheit model = (\n -> ((n * 9) `quot` 5) + 32) <$> celsius model

celsius :: TempModel (DynamicF Gtk) -> Dynamic Gtk Int
celsius model = unF $ modelCelsius model

setFahreinheit :: TempModel UpdateF -> Int -> IO ()
setFahreinheit m n = setCelsius m $ ((n - 32) * 5) `quot` 9

setCelsius :: TempModel UpdateF -> Int -> IO ()
setCelsius (TempModel m) = update m

data AppEvent = SetCelsius Int | SetFahreinheit Int | Search Text

handleEvent :: AppEvent -> TempModel UpdateF -> IO ()
handleEvent (SetFahreinheit n) m = setFahreinheit m n
handleEvent (SetCelsius n) m =  setCelsius m n
handleEvent (Search t) m = print t

renderGUI :: TempModel (DynamicF Gtk) -> Markup Gtk Root AppEvent
renderGUI model =
  column
    [ "Celsius",
      (fmap :: (Int -> AppEvent) -> Markup Gtk Block Int -> Markup Gtk Block AppEvent) SetCelsius $
        numberField (celsius model),
      "Fahreinheit",
      SetFahreinheit <$> numberField (fahreinheit model),
      searchComponent
    ]

app :: App TempModel Gtk AppEvent
app =
  App
    { appRender = renderGUI,
      appHandleEvent = handleEvent,
      appInitialState = TempModel $ IdentityF 0
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

