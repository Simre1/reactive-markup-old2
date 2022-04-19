{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module ReactiveMarkup.Target.Gtk (Gtk, MakeGtk (..), runGtk) where

import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.RHKT
  ( ApplyF,
    F (..),
    FunctorF (FunctorF),
    ID (..),
    ZipTraverseF,
    foldF2,
    mapF,
    traverseF,
  )
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import ReactiveMarkup.App
  ( App (appHandleEvent, appInitialState, appRender),
    Model (Model),
    Update (UpdateKeep, UpdatePropagate, UpdateSet),
    UpdateF (..),
    appName,
    getInternalModel,
  )
import ReactiveMarkup.Markup (Dynamic, renderMarkup)
import ReactiveMarkup.Target.Gtk.Base
import ReactiveMarkup.Target.Gtk.Container
import ReactiveMarkup.Target.Gtk.Inline
import ReactiveMarkup.Target.Gtk.Interactive
import ReactiveMarkup.Target.Gtk.State
import ReactiveMarkup.Target.Gtk.Styling
import qualified SimpleEvents as SE
import ReactiveMarkup.Target.Gtk.ModelF

-- onDifferentName :: s -> (s -> IO ()) -> IO (s -> IO ())
-- onDifferentName s f = do
--   stableNameRef <- makeStableName s >>= newIORef
--   pure $ \newS -> do
--     newStableName <- makeStableName newS
--     oldStableName <- readIORef stableNameRef
--     when (oldStableName /= newStableName) $ do
--       writeIORef stableNameRef oldStableName
--       f newS

runGtk :: ZipTraverseF s => App Gtk s e -> IO ()
runGtk app = do
  model <- initiateModel (appInitialState app)

  let makeWidget = makeGtk (renderMarkup (appRender app $ modelToDynamic model)) $ \e -> do
        state <- modelToUpdate model
        updatedModel <- appHandleEvent app e (Model state)
        updateModel model (getInternalModel updatedModel)

  let activate gtkApp = do
        widget <- makeWidget
        window <-
          Gtk.new
            Gtk.ApplicationWindow
            [ #application Gtk.:= gtkApp,
              #title Gtk.:= appName app,
              #child Gtk.:= widget
            ]
        maybeDisplay <- Gtk.get window #display
        styleProvider <- gtkStyleProvider
        maybe (pure ()) 
          (\display ->Gtk.styleContextAddProviderForDisplay display styleProvider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER))
          maybeDisplay
        #show window

  app <-
    Gtk.new
      Gtk.Application
      [ #applicationId Gtk.:= appName app,
        Gtk.On #activate (activate ?self)
      ]

  void $ #run app Nothing

-- modelToUpdateF :: ZipTraverseF x => x ModelF -> IO (x (UpdateF Update))
-- modelToUpdateF = traverseF fD fN
--   where
--     fD :: ModelF (Direct a) -> IO (UpdateF Update (Direct a))
--     fD (ModelF d t) = do
--       v <- unsafeInterleaveIO $ SE.current $ SE.toBehavior d
--       pure $ UpdateF v (makeUpdate t)
--     fN :: ZipTraverseF f => ModelF (Nested f) -> IO (UpdateF Update (Nested f))
--     fN (ModelF d t) = do
--       v <- unsafeInterleaveIO $ SE.current (SE.toBehavior d) >>= modelToUpdateF
--       pure $ UpdateF v (makeUpdate t)
