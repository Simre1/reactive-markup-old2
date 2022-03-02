{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs, StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where
import Data.Proxy

main :: IO ()
main = putStrLn "Hello, Haskell!"

class Widget widget target context where
  toWidget :: widget target context -> target context

type WidgetKind = (* -> *) -> * -> *

data Inline

data Block

data Window

type Button :: WidgetKind
data Button c t = Button String

type Text :: WidgetKind
data Text c t = Text String

data List c t = List [Markup c t]


data Output a = Output {unO :: Output' a}

type family Test a

type instance Test () = ()

type family Output' a where
  Output' Inline = String

instance Widget Text Inline Output where
  toWidget (Text t) = Output t

instance Widget Button Inline Output where
  toWidget (Button s) = Output $ "<" <> s <> ">"

data Markup context target = forall w. Widget w context target => Markup (w context target)

markup :: (Widget w ctx target) => w ctx target -> Markup ctx target
markup = Markup

text :: (Widget Text ctx target) => String -> Markup ctx target
text s = markup (Text s)

runMarkup :: Markup context target -> target context
runMarkup (Markup elem) = toWidget elem

test :: Markup Inline Output
test = text "Hello"


