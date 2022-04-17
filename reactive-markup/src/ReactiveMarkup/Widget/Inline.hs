module ReactiveMarkup.Widget.Inline where

import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import ReactiveMarkup.Context (Inline)
import ReactiveMarkup.Markup (Markup, Render, markup)

newtype Words e = Words Text

text :: Render Words target context => Text -> Markup target context e
text = markup . Words

string :: Render Words t c => String -> Markup t c e
string = markup . Words . pack

instance Render Words t c => IsString (Markup t c e) where
  fromString = string

newtype Italic t c e = Italic (Markup t c e)

italic :: Render (Italic t Inline) t c => Markup t Inline e -> Markup t c e
italic = markup . Italic

newtype Bold t c e = Bold (Markup t c e)

bold :: Render (Bold t Inline) t c => Markup t Inline e -> Markup t c e
bold = markup . Bold