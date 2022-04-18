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

type instance RenderTarget Gtk (Padded c) = RenderTarget Gtk c

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
  ( RenderErrorOnEqual (RenderTarget Gtk (Padded c) e) (MakeGtk e) (Margin Gtk c) Gtk c,
    RenderTarget Gtk (Padded c) e ~ MakeGtk e
  ) =>
  Render (Margin Gtk c) Gtk c
  where
  render (Margin pv m) = MakeGtk $ \handle -> do
    widget <- makeGtk (renderMarkup m) handle
    let toClass direction v = maybe "" (\s -> "margin-" <> direction <> "-" <> sizeToClass s) v
    appendClasses widget $
      [ toClass "top" (marginValuesTop pv),
        toClass "bottom" (marginValuesBottom pv),
        toClass "left" (marginValuesLeft pv),
        toClass "right" (marginValuesRight pv)
      ]
    pure widget

css :: ByteString
css = marginCss
  where
    marginCss =
      let sizesCombinatedWithDirections = (,) <$> sizes <*> directions
       in flip foldMap sizesCombinatedWithDirections $ \((size, px), direction) ->
            [fmt|.margin-{direction}-{size} {{ margin-{direction}: {px}px; }} \n|]
    sizes :: [(ByteString, Int)]
    sizes = [("very-small", 2), ("small", 5), ("medium", 10), ("big", 20), ("very-big", 40)]
    directions :: [ByteString]
    directions = ["top", "bottom", "left", "right"]
