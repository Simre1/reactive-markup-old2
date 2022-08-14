module ReactiveMarkup.Widget.Paragraph where

import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import ReactiveMarkup.Context (Paragraph)
import ReactiveMarkup.Markup (Markup, Render, markup)

newtype Words e = Words Text

text :: Render Words backend context => Text -> Markup backend context e
text = markup . Words

string :: Render Words t c => String -> Markup t c e
string = markup . Words . pack

instance Render Words t c => IsString (Markup t c e) where
  fromString = string

newtype Italic t c e = Italic (Markup t c e)

italic :: Render (Italic t Paragraph) t c => Markup t Paragraph e -> Markup t c e
italic = markup . Italic

newtype Bold t c e = Bold (Markup t c e)

bold :: Render (Bold t Paragraph) t c => Markup t Paragraph e -> Markup t c e
bold = markup . Bold