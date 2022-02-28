module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"


class Widget widget context target where
  type TargetOutput context target
  toWidget :: widget context target -> TargetOutput context target

class SubContext super sub where
  superContext :: widget sub target -> widget super target


instance {-# OVERLAPPABLE #-} SubContext same same where
  superContext = id

data Inline

data Block

data Button c t = Button String

data Text c t = c ~ Inline => Text String

data List c t = List [Markup c t]

instance Widget Text Inline String where
  type TargetOutput Inline String = String
  toWidget (Text t) = t

instance Widget Button Inline String where
  type TargetOutput Inline String = String
  toWidget (Button s) = "<" <> s <> ">"

data Markup context target = forall w. Widget w context target => Markup (w context target)

markup :: forall super sub target w. (SubContext super sub, Widget w sub target) => w sub target -> Markup super target
markup = superContext . Markup

text :: (SubContext super Inline, Widget Text Inline target) => String -> Markup super target
text s = markup $ Text s 

runMarkup :: forall target context. Markup context target -> TargetOutput context target
runMarkup (Markup elem) = toWidget elem

test :: Widget Text Inline a => Markup Inline a
test = text "Hello"
