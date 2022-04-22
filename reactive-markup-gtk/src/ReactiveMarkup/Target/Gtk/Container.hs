module ReactiveMarkup.Target.Gtk.Container where

import Control.Monad
import qualified Data.Text as T
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
import Data.IORef
import qualified Data.IntMap as IM
import Control.Monad.IO.Class

box :: MakeGtkRender (Column Gtk c) c e => Gtk.Orientation -> [Markup Gtk c e] -> MakeGtk e
box orientation ms = MakeGtk $ do
    box <- Gtk.boxNew orientation 0
    
    let markups = zip [0..] ms
    children <- liftIO $ newIORef $ IM.empty
    let setWidget i w = do
          oldWidget <- atomicModifyIORef children (\im -> (IM.insert i w im, IM.lookup i im))
          previousWidget <- IM.lookup (pred i) <$> readIORef children
          maybe mempty (\w -> Gtk.boxRemove box w) oldWidget
          Gtk.boxInsertChildAfter box w previousWidget

    forM_ markups $ \(i, m) -> do
      localSetWidget (setWidget i) $
        makeGtk (renderMarkup m)
    
    Gtk.toWidget box >>= setWidgetNow

instance MakeGtkRender (Column Gtk c) c e => Render (Column Gtk Block) Gtk c where
  render (Column ms) = box Gtk.OrientationVertical ms 

instance MakeGtkRender (Row Gtk c) c e => Render (Row Gtk Block) Gtk c where
  render (Row ms) = box Gtk.OrientationHorizontal ms 
