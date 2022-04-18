{-# LANGUAGE ViewPatterns #-}
module ReactiveMarkup.Widget.Styling where
import ReactiveMarkup.Markup

data Padded c

data Size = VerySmall | Small | Medium | Big | VeryBig

data MarginValues = MarginValues {
  marginValuesTop :: Maybe Size,
  marginValuesBottom :: Maybe Size,
  marginValuesLeft :: Maybe Size,
  marginValuesRight :: Maybe Size
}

data Margin t c e = Margin MarginValues (Markup t (Padded c) e)

margin :: Render (Margin t c) t c => Size -> Markup t (Padded c) e -> Markup t c e
margin (Just -> p) = markup . Margin (MarginValues p p p p)