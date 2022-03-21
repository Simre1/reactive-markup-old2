module Main where

import Data.Void
import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk
import ReactiveMarkup.Widgets.Base
import ReactiveMarkup.Widgets.Eventful
import ReactiveMarkup.App
import Data.Text as T

import Data.Default
import qualified Data.Text.Read as T
import Optics.Core
import Optics.Generic
import Debug.Trace


main = do
  runGtk app

data Model = Celsius Int deriving Show

fahreinheit :: Model -> Int
fahreinheit (Celsius n) = (n * 9 `quot` 5) + 32

celsius :: Model -> Int
celsius (Celsius n) = n

setFahreinheit :: Int -> Model -> Model
setFahreinheit n _ = Celsius $ (n - 32) * 5 `quot` 9

setCelsius :: Int -> Model -> Model
setCelsius n _ = Celsius n

data AppEvent = SetCelsius Int | SetFahreinheit Int

app :: App Model Gtk AppEvent
app = App
  { appRender = renderGUI
  , appHandleEvent = (pure .) . handleEvent
  , appInitialState = Celsius 0
  }

handleEvent :: AppEvent -> Model -> Model
handleEvent (SetFahreinheit n) m = setFahreinheit n m
handleEvent (SetCelsius n) m = setCelsius n m


numberField :: Dynamic Gtk Int -> Markup Gtk Block Int
numberField state = 
  let text :: Dynamic Gtk Text = pack <$> show <$> state
  in filterEvents parseNumber $ textField $ (#value .~ text) . (#change .~ Just id)
  where
    parseNumber :: TextFieldEvent -> Maybe Int
    parseNumber (TextFieldEvent t) = either (const Nothing) (\(num, t) -> if T.null t then Just num else Nothing) $ T.signed T.decimal $ t


renderGUI :: Dynamic Gtk Model -> Markup Gtk Root AppEvent
renderGUI model = blocks
  [ "Celsius"
  , SetCelsius <$> numberField (celsius <$> model)
  , "Fahreinheit"
  , SetFahreinheit <$> numberField (fahreinheit <$> model)
  ]






-- localState update initialState $ \dI ->
--   blocks
--     [ dynamicMarkup dI (text),
--       textField (\d -> d {activate = Just id, value = dI}),
--       textField (\d -> d {activate = Just id, value = dI})
--     ]
--   where
--     myButton :: Markup Gtk Block ButtonClick
--     myButton = button "Click"
--     clicked :: Int -> Markup Gtk Inline e
--     clicked times = "You have clicked " <> bold (string (show times)) <> " times."

--     update :: Text -> TextFieldEvent -> (Maybe Text, Maybe AppEvent)
--     update _ (TextFieldEvent t) = (Just t, Nothing)

--     initialState :: Text
--     initialState = "Hello World"
