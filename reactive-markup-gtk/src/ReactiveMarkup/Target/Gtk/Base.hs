{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module ReactiveMarkup.Target.Gtk.Base where

import Control.Monad
import Data.Functor.Const
import Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import GI.Pango.Functions ()
import ReactiveMarkup.Context
import ReactiveMarkup.Markup
import ReactiveMarkup.Widget
import qualified SimpleEvents as SE

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

data Gtk

type instance RenderTarget Gtk Inline = Const Text

newtype GtkContext e a = GtkContext {runGtkContext :: ReaderT (e -> IO (), Gtk.Widget -> IO ()) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

newtype MakeGtk e = MakeGtk {makeGtk :: GtkContext e ()}

askSetWidget :: GtkContext e (Gtk.Widget -> IO ())
askSetWidget = GtkContext $ snd <$> ask

setWidgetNow :: Gtk.Widget -> GtkContext e ()
setWidgetNow w = askSetWidget >>= \f -> liftIO (f w)

askHandleEvent :: GtkContext e (e -> IO ())
askHandleEvent = GtkContext $ fst <$> ask

localSetWidget :: (Gtk.Widget -> IO ()) -> GtkContext e a -> GtkContext e a
localSetWidget setWidget (GtkContext c) = do
  handleEvent <- askHandleEvent
  GtkContext $ local (handleEvent, setWidget) c

localHandleEvent :: (e -> IO ()) -> GtkContext e a -> GtkContext e' a
localHandleEvent handleEvent (GtkContext c) = do
  setWidget <- askSetWidget
  GtkContext $ local (handleEvent, setWidget) c

type instance RenderTarget Gtk Block = MakeGtk

type instance RenderTarget Gtk Root = MakeGtk

newtype instance Dynamic Gtk a = GtkDynamic (SE.Dynamic a) deriving (Functor, Applicative, Monad)

type MakeGtkRender w c e = (RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (w e) Gtk c,
    RenderTarget Gtk c e ~ MakeGtk e)

instance MakeGtkRender (FilterEvents Gtk c) c e => Render (FilterEvents Gtk c) Gtk c where
  render (FilterEvents f m) = MakeGtk $ do
    handle <- askHandleEvent
    localHandleEvent (newF handle) $ makeGtk (renderMarkup m)
    where
      newF h e = maybe (pure ()) h (f e)

instance Render (Lift Gtk Block) Gtk Root where
  render (Lift m) = renderMarkup m

instance Render (Lift Gtk Inline) Gtk Block where
  render (Lift m) = pangoToWidget $ getConst $ renderMarkup m

instance MakeGtk ~ RenderTarget c t => Render (Map c t) c t where
  render (Map f m) = MakeGtk $ \handle -> makeGtk (renderMarkup m) (handle . f)

whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = do
  b <- c
  when b a

pangoToWidget :: Text -> MakeGtk e
pangoToWidget t = MakeGtk $ do
  label <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label t
  Gtk.toWidget label >>= setWidgetNow