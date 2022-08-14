module ReactiveMarkup.Widget.Container where

import ReactiveMarkup.Context (Common)
import ReactiveMarkup.Markup (Markup, Render, markup)

newtype Row t c e = Row [Markup t c e]

row :: Render (Row t Common) t c => [Markup t Common e] -> Markup t c e
row = markup . Row

newtype Column t c e = Column [Markup t c e]

column :: Render (Column t Common) t c => [Markup t Common e] -> Markup t c e
column = markup . Column