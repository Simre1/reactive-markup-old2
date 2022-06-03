{-# LANGUAGE QuasiQuotes #-}

module ReactiveMarkup.Target.Gtk.Styling where

import Control.Monad
import Data.ByteString
import Data.Maybe
import Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import GI.Pango.Functions ()
import PyF
import ReactiveMarkup.Context
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk.Base
import ReactiveMarkup.Widget

import qualified SimpleEvents as SE
import Optics.Core

-- type instance RenderTarget Gtk (Padded c) = RenderTarget Gtk c

appendClasses :: Gtk.Widget -> [Text] -> IO ()
appendClasses widget newClasses = do
  classes <- fromMaybe [] <$> Gtk.get widget #cssClasses
  Gtk.set widget [#cssClasses Gtk.:= classes <> newClasses]

gtkStyleProvider :: IO Gtk.StyleProvider
gtkStyleProvider = do
  cssProvider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData cssProvider css
  Gtk.toStyleProvider cssProvider

sizeToClass :: Size -> Text
sizeToClass VeryBig = "very-big"
sizeToClass Big = "big"
sizeToClass Medium = "medium"
sizeToClass Small = "small"
sizeToClass VerySmall = "very-small"

instance
  ( RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (Margin Gtk c) Gtk c,
    RenderTarget Gtk c e ~ MakeGtk e
  ) =>
  Render (Margin Gtk c) Gtk c
  where
  render (Margin pv m) = MakeGtk $ do
    setWidget <- askSetWidget
    let toClass direction v = maybe "" (\s -> "margin-" <> direction <> "-" <> sizeToClass s) v
        classes =
          [ toClass "top" (pv ^. #top),
            toClass "bottom" (pv ^. #bottom),
            toClass "left" (pv ^. #left),
            toClass "right" (pv ^. #right)
          ]
    localSetWidget (\w -> appendClasses w classes >> setWidget w) $ 
      makeGtk (renderMarkup m)


instance
  ( RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (Border Gtk c) Gtk c,
    RenderTarget Gtk c e ~ MakeGtk e
  ) =>
  Render (Border Gtk c) Gtk c
  where
  render (Border pv m) = MakeGtk $ do
    setWidget <- askSetWidget
    let toClass direction v = maybe "" (\s -> "border-" <> direction <> "-" <> sizeToClass s) v
        classes =
          [ toClass "top" (pv ^. #top),
            toClass "bottom" (pv ^. #bottom),
            toClass "left" (pv ^. #left),
            toClass "right" (pv ^. #right)
          ]
    localSetWidget (\w -> appendClasses w classes >> setWidget w) $ 
      makeGtk (renderMarkup m)



css :: ByteString
css = marginCss <> borderCss
  where
    marginCss =
      let sizesCombinatedWithDirections = (,) <$> marginSizes <*> directions
       in flip foldMap sizesCombinatedWithDirections $ \((size, px), direction) ->
            [fmt|.margin-{direction}-{size} {{ margin-{direction}: {px}px; }} \n|]
    borderCss =
      let sizesCombinatedWithDirections = (,) <$> borderSizes <*> directions
       in flip foldMap sizesCombinatedWithDirections $ \((size, px), direction) ->
            [fmt|.border-{direction}-{size} {{ border-{direction}: solid {px}px; }} \n|]
    borderSizes :: [(ByteString, Int)]
    borderSizes = [("very-small", 1), ("small", 2), ("medium", 4), ("big", 8), ("very-big", 16)]
    marginSizes :: [(ByteString, Int)]
    marginSizes = [("very-small", 2), ("small", 5), ("medium", 10), ("big", 20), ("very-big", 40)]
    directions :: [ByteString]
    directions = ["top", "bottom", "left", "right"]
