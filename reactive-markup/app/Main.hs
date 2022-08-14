module Main where

import ReactiveMarkup.Markup
import ReactiveMarkup.Widgets.Base

import ReactiveMarkup.Widgets.Eventful
import ReactiveMarkup.Contexts.Base

import ReactiveMarkup.Target.Html.Render

import ReactiveMarkup.Target.Html.Javascript
import qualified Data.Text.Lazy.IO as T

import Text.Blaze.Html.Renderer.Text
import Data.Text

main = do
  T.putStrLn $ renderHtml $ runHtml jsLog test


test :: Markup Html Common Text
test = list [emphasis $ string "Hello", mapEvent (jsConst $ toJSVal @Text "hello world") $ button (emphasis (string "really"))]


