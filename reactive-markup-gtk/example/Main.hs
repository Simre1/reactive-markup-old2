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

newtype TempModel = Celsius Int deriving Show

fahreinheit :: TempModel -> Int
fahreinheit (Celsius n) = (n * 9 `quot` 5) + 32

celsius :: TempModel -> Int
celsius (Celsius n) = n

setFahreinheit :: Int -> TempModel -> TempModel
setFahreinheit n _ = Celsius $ (n - 32) * 5 `quot` 9

setCelsius :: Int -> TempModel -> TempModel
setCelsius n _ = Celsius n

data AppEvent = SetCelsius Int | SetFahreinheit Int

handleEvent :: AppEvent -> TempModel -> TempModel
handleEvent (SetFahreinheit n) m = setFahreinheit n m
handleEvent (SetCelsius n) m = setCelsius n m

renderGUI :: Dynamic Gtk TempModel -> Markup Gtk Root AppEvent
renderGUI model = blocks
  [ "Celsius"
  , (fmap :: (Int -> AppEvent) -> Markup Gtk Block Int -> Markup Gtk Block AppEvent) SetCelsius $ numberField 
    ((fmap :: (TempModel -> Int) -> Dynamic Gtk TempModel -> Dynamic Gtk Int) celsius model)
  , "Fahreinheit"
  , SetFahreinheit <$> numberField (fahreinheit <$> model)
  ]

app :: App TempModel Gtk AppEvent
app = App
  { appRender = renderGUI
  , appHandleEvent = \model event -> (pure :: TempModel -> IO TempModel) (handleEvent model event)
  , appInitialState = Celsius 0
  }

numberField :: Dynamic Gtk Int -> Markup Gtk Block Int
numberField state =
  let text :: Dynamic Gtk Text = pack . show <$> state
  in filterEvents parseNumber $ textField $ (#value .~ text) . (#change ?~ id)
  where
    parseNumber :: TextFieldEvent -> Maybe Int
    parseNumber (TextFieldEvent t) = either (const Nothing) (\(num, t) -> 
      if T.null t then Just num else Nothing) $ T.signed T.decimal $ t


