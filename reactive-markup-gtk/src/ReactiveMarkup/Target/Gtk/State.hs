module ReactiveMarkup.Target.Gtk.State where

import Control.Monad
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
import ReactiveMarkup.Target.Gtk.Base
import Data.IORef
import ReactiveMarkup.Target.Gtk.ModelF
import Data.Coerce
import ReactiveMarkup.Update


instance MakeGtkRender (DynamicMarkup s Gtk c) c e => Render (DynamicMarkup s Gtk c) Gtk c where
  render (DynamicMarkup dynamicState makeMarkup) = MakeGtk $ \handleEvent -> do
    frame <- Gtk.boxNew Gtk.OrientationVertical 0
    -- Gtk.frameSetShadowType frame Gtk.ShadowTypeNone
    state <- SE.current $ SE.toBehavior (coerce dynamicState)

    cleanUpRef <- newIORef (pure ())
    let setWidget widget = do
          cleanUp <- join $ readIORef cleanUpRef
          writeIORef cleanUpRef (Gtk.boxRemove frame widget)
          Gtk.boxAppend frame widget

        -- #showAll widget
        generateWidget state = do
          !s <- pure state
          makeGtk (renderMarkup (makeMarkup s)) handleEvent

    let handler newState = do
          w <- generateWidget newState
          setWidget w

    unregisterWidgetUpdate <-
      SE.reactimate (SE.toEvent $ coerce dynamicState) $ SE.simpleEventHandler handler
    generateWidget state >>= setWidget
    widget <- Gtk.toWidget frame
    Gtk.on widget #destroy (SE.liftES unregisterWidgetUpdate)
    pure widget

instance MakeGtkRender (LocalState s Gtk c) c e => Render (LocalState s Gtk c) Gtk c where
  render (LocalState update initial makeMarkup) = MakeGtk $ \handleOuterEvent -> do
    model <- initiateModel initial
    let handleInnerEvent innerEvent = do
          modelUpdate <- modelToUpdate model
          let LocalUpdate modelUpdate' outerEvent = update innerEvent (LocalUpdate (Model modelUpdate) Nothing)
          updateModel model (getInternalModel modelUpdate')
          maybe (pure ()) handleOuterEvent outerEvent
    makeGtk (renderMarkup $ makeMarkup (modelToDynamic model)) handleInnerEvent

instance MakeGtkRender (Counter Gtk c) c e => Render (Counter Gtk c) Gtk c where
  render (Counter intervall f) = MakeGtk $ \handle -> do
    (d, t) <- SE.newDynamic 0
    GLib.timeoutAdd GLib.PRIORITY_DEFAULT (round (intervall * 1000)) $ do
      SE.current (SE.toBehavior d) >>= SE.triggerEvent t . succ
      pure True
    -- Gtk.on widget #destroy $ killThread thread
    makeGtk (renderMarkup (f $ GtkDynamic d)) handle


instance MakeGtkRender (MapEventIO Gtk c) c e => Render (MapEventIO Gtk c) Gtk c where
  render (MapEventIO f m) = MakeGtk $ \handle ->
    makeGtk (renderMarkup m) (\e -> f e >>= maybe (pure ()) handle)