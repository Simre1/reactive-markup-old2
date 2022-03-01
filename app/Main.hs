{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where
import Data.Proxy

main :: IO ()
main = putStrLn "Hello, Haskell!"


class Widget widget context target where
  toWidget :: context -> widget context target -> target

class SubContext super sub where
  subContext :: super -> sub

instance SubContext same same where
  subContext = id

data Inline = Inline String

data Block = Block

data Window = Window

data Web = Web

data Button c t = Button String

data Text c t = c ~ Inline => Text String

data List c t = List [Markup c t]

instance Widget Text Inline String where
  toWidget (Inline str) (Text t) = str <> t

instance Widget Button Inline String where
  toWidget _ (Button s) = "<" <> s <> ">"

data Markup context target = forall w hiddenContext. Widget w hiddenContext target => Markup (context -> hiddenContext) (w hiddenContext target)

markup :: forall super sub target w b. (SubContext super sub, Widget w sub target) => w sub target -> Markup super target
markup = Markup subContext

text :: (SubContext super sub, Widget Text sub target) => String -> Markup super target
text s = markup (Text s)

runMarkup :: forall target context. context -> Markup context target -> target
runMarkup context (Markup f elem) = toWidget (f context) elem

test :: Widget Text Inline a => Markup Inline a
test = text "Hello"

-- transitive :: Widget Text Inline a => Markup Web a
-- transitive = text "Hello"


