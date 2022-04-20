module ReactiveMarkup.Target.Gtk.Interactive where

import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Functor.Const
import Data.IORef
import Data.Text as T
import Data.Void
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
import Control.Monad.IO.Class

instance MakeGtkRender (Button Gtk c) c e => Render (Button Gtk Inline) Gtk c where
  render (Button t (ButtonOptions clickF)) = MakeGtk $ do
    button <- Gtk.buttonNew
    handleEvent <- askHandleEvent
    localSetWidget (\w -> Gtk.buttonSetChild button (Just w)) $
      localHandleEvent absurd $ 
        makeGtk (pangoToWidget $ getConst $ renderMarkup t)
    maybe (pure ()) (\e -> void $ Gtk.onButtonClicked button $ handleEvent e) clickF
    Gtk.toWidget button >>= setWidgetNow

instance MakeGtkRender (TextField Gtk) c e => Render (TextField Gtk) Gtk c where
  render (TextField (TextFieldOptions value handleActivate handleChange)) = MakeGtk $ do
    handleEvent <- askHandleEvent
    entry <- Gtk.entryNew
    entryBuffer <- Gtk.entryGetBuffer entry
    currentValue <- liftIO $ SE.current $ SE.toBehavior $ SE.onlyTriggerOnChange $ coerce value
    Gtk.setEntryBufferText entryBuffer currentValue

    active <- liftIO $ newIORef True
    let protect a = do
          whenM (readIORef active) $ do
            a

    sequenceA_ $ (\handle -> Gtk.after entry #changed $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle) <$> handleChange

    sequenceA_ $ (\handle -> Gtk.onEntryActivate entry $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle) <$> handleActivate
    -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferDeletedText entryBuffer $ \_ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange
    -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferInsertedText entryBuffer $ \_ _ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange

    let update = \newText -> do
          writeIORef active False
          p <- Gtk.get entry #cursorPosition
          Gtk.setEntryBufferText entryBuffer newText
          Gtk.editableSetPosition entry p
          writeIORef active True

    liftIO $ SE.reactimate (SE.toEvent $ coerce value) $ SE.simpleEventHandler update

    Gtk.toWidget entry >>= setWidgetNow