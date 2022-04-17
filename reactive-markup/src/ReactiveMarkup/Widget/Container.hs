module ReactiveMarkup.Widget.Container where

import ReactiveMarkup.Context (Block)
import ReactiveMarkup.Markup (Markup, Render, markup)

newtype Row t c e = Row [Markup t c e]

row :: Render (Row t Block) t c => [Markup t Block e] -> Markup t c e
row = markup . Row

newtype Column t c e = Column [Markup t c e]

column :: Render (Column t Block) t c => [Markup t Block e] -> Markup t c e
column = markup . Column