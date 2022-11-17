module ReactiveMarkup.Widget.Paragraph where

import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import ReactiveMarkup.Context (Paragraph)
import ReactiveMarkup.Markup (Markup, Render, wrapMarkup)

newtype Words e = Words Text

text :: Render Words backend context => Text -> Markup backend context e
text = wrapMarkup . Words

string :: Render Words t c => String -> Markup t c e
string = wrapMarkup . Words . pack

instance Render Words t c => IsString (Markup t c e) where
  fromString :: Render Words t c => String -> Markup t c e
  fromString = string

newtype Italic t c e = Italic (Markup t c e)

italic :: Render (Italic t Paragraph) t c => Markup t Paragraph e -> Markup t c e
italic = wrapMarkup . Italic

newtype Bold t c e = Bold (Markup t c e)

bold :: Render (Bold t Paragraph) t c => Markup t Paragraph e -> Markup t c e
bold = wrapMarkup . Bold