{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module ReactiveMarkup.Widgets.Base where

import Control.Applicative
import Data.Text (Text, pack)
import ReactiveMarkup.Markup
import ReactiveMarkup.Contexts.Base
import Data.String


-- Lift

data Lift t c e = Lift (Markup t c e)

lift :: forall c1 c2 t e. Render (Lift t c1) t c2 => Markup t c1 e -> Markup t c2 e
lift = markup . Lift

-- Text widgets

data Words e = Words Text

text :: Render Words target context => Text -> Markup target context e
text = markup . Words

string :: Render Words t c => String -> Markup t c e
string = markup . Words . pack

instance Render Words t c => IsString (Markup t c e) where
  fromString = string

data Emphasis t c e = Emphasis (Markup t c e)

emphasis :: Render (Emphasis t Inline) t c => Markup t Inline e -> Markup t c e
emphasis = markup . Emphasis

data Bold t c e = Bold (Markup t c e)

bold :: Render (Bold t Inline) t c => Markup t Inline e -> Markup t c e
bold = markup . Bold

-- Multiple Elements

data Blocks t c e = Blocks [Markup t c e]

blocks :: Render (Blocks t Block) t c => [Markup t Block e] -> Markup t c e
blocks = markup . Blocks
