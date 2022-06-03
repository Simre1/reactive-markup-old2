{-# LANGUAGE ViewPatterns #-}
module ReactiveMarkup.Widget.Styling where
import ReactiveMarkup.Markup
import GHC.Generics

-- data Padded c

data Size = VerySmall | Small | Medium | Big | VeryBig

data MarginValues = MarginValues {
  top :: Maybe Size,
  bottom :: Maybe Size,
  left :: Maybe Size,
  right :: Maybe Size
} deriving Generic

data Margin t c e = Margin MarginValues (Markup t c e)

margin :: Render (Margin t c) t c => Size -> Markup t c e -> Markup t c e
margin (Just -> p) = markup . Margin (MarginValues p p p p)

marginTop :: Render (Margin t c) t c => Size -> Markup t c e -> Markup t c e
marginTop (Just -> p) = markup . Margin (MarginValues p Nothing Nothing Nothing)

marginBottom :: Render (Margin t c) t c => Size -> Markup t c e -> Markup t c e
marginBottom (Just -> p) = markup . Margin (MarginValues Nothing p Nothing Nothing)

marginRight :: Render (Margin t c) t c => Size -> Markup t c e -> Markup t c e
marginRight (Just -> p) = markup . Margin (MarginValues Nothing Nothing Nothing p)

marginLeft :: Render (Margin t c) t c => Size -> Markup t c e -> Markup t c e
marginLeft (Just -> p) = markup . Margin (MarginValues Nothing Nothing p Nothing)

marginHorizontal :: Render (Margin t c) t c => Size -> Markup t c e -> Markup t c e
marginHorizontal (Just -> p) = markup . Margin (MarginValues Nothing Nothing p p)

marginVertical :: Render (Margin t c) t c => Size -> Markup t c e -> Markup t c e
marginVertical (Just -> p) = markup . Margin (MarginValues p p Nothing Nothing)


data Border t c e = Border BorderValues (Markup t c e)

data BorderValues = BorderValues {
  top :: Maybe Size,
  bottom :: Maybe Size,
  left :: Maybe Size,
  right :: Maybe Size
} deriving Generic

border :: Render (Border t c) t c => Size -> Markup t c e -> Markup t c e
border (Just -> p) = markup . Border (BorderValues p p p p)

borderTop :: Render (Border t c) t c => Size -> Markup t c e -> Markup t c e
borderTop (Just -> p) = markup . Border (BorderValues p Nothing Nothing Nothing)

borderBottom :: Render (Border t c) t c => Size -> Markup t c e -> Markup t c e
borderBottom (Just -> p) = markup . Border (BorderValues Nothing p Nothing Nothing)

borderRight :: Render (Border t c) t c => Size -> Markup t c e -> Markup t c e
borderRight (Just -> p) = markup . Border (BorderValues Nothing Nothing Nothing p)

borderLeft :: Render (Border t c) t c => Size -> Markup t c e -> Markup t c e
borderLeft (Just -> p) = markup . Border (BorderValues Nothing Nothing p Nothing)

borderHorizontal :: Render (Border t c) t c => Size -> Markup t c e -> Markup t c e
borderHorizontal (Just -> p) = markup . Border (BorderValues Nothing Nothing p p)

borderVertical :: Render (Border t c) t c => Size -> Markup t c e -> Markup t c e
borderVertical (Just -> p) = markup . Border (BorderValues p p Nothing Nothing)
