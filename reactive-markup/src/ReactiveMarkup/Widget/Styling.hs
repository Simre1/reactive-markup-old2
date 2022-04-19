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

marginTop :: Render (Margin t c) t c => Size -> Markup t (Padded c) e -> Markup t c e
marginTop (Just -> p) = markup . Margin (MarginValues p Nothing Nothing Nothing)

marginBottom :: Render (Margin t c) t c => Size -> Markup t (Padded c) e -> Markup t c e
marginBottom (Just -> p) = markup . Margin (MarginValues Nothing p Nothing Nothing)

marginRight :: Render (Margin t c) t c => Size -> Markup t (Padded c) e -> Markup t c e
marginRight (Just -> p) = markup . Margin (MarginValues Nothing Nothing Nothing p)

marginLeft :: Render (Margin t c) t c => Size -> Markup t (Padded c) e -> Markup t c e
marginLeft (Just -> p) = markup . Margin (MarginValues Nothing Nothing p Nothing)

marginHorizontal :: Render (Margin t c) t c => Size -> Markup t (Padded c) e -> Markup t c e
marginHorizontal (Just -> p) = markup . Margin (MarginValues Nothing Nothing p p)

marginVertical :: Render (Margin t c) t c => Size -> Markup t (Padded c) e -> Markup t c e
marginVertical (Just -> p) = markup . Margin (MarginValues p p Nothing Nothing)
