module ReactiveMarkup.Widgets.Base where

import Control.Applicative
import Data.Text (Text, pack)
import ReactiveMarkup.Widget

-- Text widgets

data Words :: Widget where
  Words :: Text -> Words t c

text :: Render Words t c => Text -> Markup t c
text = markup . Words

string :: Render Words t c => String -> Markup t c
string = markup . Words . pack

data Emphasis i t c = Emphasis ()

emphasis :: Markup t c -> Markup t c
emphasis = id

-- Multiple Elements

data List :: Widget where
  List :: [Markup t c] -> List t c

list :: Render List t c => [Markup t c] -> Markup t c
list = markup . List
