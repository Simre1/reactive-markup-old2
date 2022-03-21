module Main where

import ReactiveMarkup.Markup
import ReactiveMarkup.Widgets.Base

import ReactiveMarkup.Widgets.Eventful
import ReactiveMarkup.Contexts.Base
import Data.Text

import ReactiveMarkup.Target.Threepenny

main :: IO ()
main = startThreepenny pure test


test :: Markup Threepenny Root ()
test = emphasis $ string "Hello" <> string " World!!!"


