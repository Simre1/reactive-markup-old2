module ReactiveMarkup.Target.Gtk.Container where

import Control.Monad
import Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import GI.Pango.Functions ()
import ReactiveMarkup.Context
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk.Base
import ReactiveMarkup.Widget
import qualified SimpleEvents as SE

instance MakeGtkRender (Column Gtk c) c e => Render (Column Gtk Block) Gtk c where
  render (Column ms) = MakeGtk $ \handle -> do
    box <- Gtk.boxNew Gtk.OrientationVertical 0
    forM_ ms $ \m -> do
      child <- makeGtk (renderMarkup m) handle
      Gtk.boxAppend box child
    Gtk.toWidget box

instance MakeGtkRender (Row Gtk c) c e => Render (Row Gtk Block) Gtk c
  where
  render (Row ms) = MakeGtk $ \handle -> do
    box <- Gtk.boxNew Gtk.OrientationHorizontal 0
    forM_ ms $ \m -> do
      child <- makeGtk (renderMarkup m) handle
      Gtk.boxAppend box child
    Gtk.toWidget box

-- instance Render (Column Gtk Block) Gtk Root where
--   render b = render @_ @Gtk @Block b

-- instance Render (Row Gtk Block) Gtk Root where
--   render b = render @_ @Gtk @Block b