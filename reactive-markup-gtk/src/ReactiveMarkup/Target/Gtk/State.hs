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
import Control.Monad.IO.Class
import Data.Functor.Identity

instance MakeGtkRender (DynamicMarkup s Gtk c) c e => Render (DynamicMarkup s Gtk c) Gtk c where
  render (DynamicMarkup dynamicState makeMarkup) = MakeGtk $ do

    (cleanUp, runCleanUp) <- liftIO makeCleanUp
    addCleanUp runCleanUp

    generateWidget <- localCleanUp cleanUp $ applyGtkContext $ \state -> do
        liftIO runCleanUp
        makeGtk (renderMarkup (makeMarkup state))

    liftIO $ SE.current (SE.toBehavior (coerce dynamicState)) >>= generateWidget

    unregisterWidgetUpdate <- liftIO $
      SE.reactimate (SE.toEvent $ coerce dynamicState) $ SE.simpleEventHandler generateWidget

    addCleanUp (SE.liftES unregisterWidgetUpdate)

    pure ()

instance MakeGtkRender (LocalState s Gtk c) c e => Render (LocalState s Gtk c) Gtk c where
  render (LocalState update initial makeMarkup) = MakeGtk $ do
    handleOuterEvent <- askHandleEvent

    model <- liftIO $ initiateModel initial

    let handleInnerEvent innerEvent = do
          modelUpdate <- modelToUpdate model
          let (outerEvent, modelUpdate') = runIdentity $ runModelM modelUpdate (update innerEvent)
          updateModel model (modelUpdate')
          maybe (pure ()) handleOuterEvent outerEvent

    localHandleEvent handleInnerEvent $
      makeGtk (renderMarkup $ makeMarkup (modelToDynamic model))

instance MakeGtkRender (Counter Gtk c) c e => Render (Counter Gtk c) Gtk c where
  render (Counter intervall f) = MakeGtk $ do
    (d, t) <- liftIO $ SE.newDynamic 0
    GLib.timeoutAdd GLib.PRIORITY_DEFAULT (round (intervall * 1000)) $ do
      SE.current (SE.toBehavior d) >>= SE.triggerEvent t . succ
      pure True
    -- Gtk.on widget #destroy $ killThread thread
    makeGtk (renderMarkup (f $ GtkDynamic d))


instance MakeGtkRender (MapEventIO Gtk c) c e => Render (MapEventIO Gtk c) Gtk c where
  render (MapEventIO f m) = MakeGtk $ do
    handleEvent <- askHandleEvent
    localHandleEvent (\e -> f e >>= maybe (pure ()) handleEvent) $
      makeGtk (renderMarkup m)