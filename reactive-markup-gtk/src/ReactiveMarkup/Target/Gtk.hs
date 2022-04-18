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
import GHC.IO (unsafeInterleaveIO)
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

data ModelF (f :: F) = ModelF (SE.Dynamic (ApplyF f ModelF)) (SE.EventTrigger (ApplyF f ID))

initiateModel :: forall x. ZipTraverseF x => x ID -> IO (x ModelF)
initiateModel = traverseF fD fN
  where
    fD :: forall a. ID (Direct a) -> IO (ModelF (Direct a))
    fD (ID a) = do
      (d, t) <- SE.newDynamic a
      pure $ ModelF d t
    fN :: forall a. ZipTraverseF a => ID (Nested a) -> IO (ModelF (Nested a))
    fN (ID a) = do
      m <- initiateModel a
      (d, t) <- SE.newDynamic m
      pure $ ModelF d $ SE.mapEventTrigger initiateModel t

modelToDynamic :: ZipTraverseF x => x ModelF -> x (FunctorF (Dynamic Gtk))
modelToDynamic = mapF (\(ModelF a _) -> FunctorF (GtkDynamic a)) (\(ModelF a _) -> FunctorF $ GtkDynamic $ modelToDynamic <$> a)

modelToUpdate :: ZipTraverseF x => x ModelF -> IO (x Update)
modelToUpdate = traverseF fD fN
  where
    fD :: ModelF (Direct a) -> IO (Update (Direct a))
    fD (ModelF d t) = unsafeInterleaveIO $ fmap UpdateKeep $ SE.current $ SE.toBehavior d
    fN :: ZipTraverseF f => ModelF (Nested f) -> IO (Update (Nested f))
    fN (ModelF d t) = unsafeInterleaveIO $ do
      a <- SE.current $ SE.toBehavior d
      UpdateKeep <$> modelToUpdate a

updateModel :: ZipTraverseF x => x ModelF -> x Update -> IO ()
updateModel = foldF2 fD fN
  where
    fD :: ModelF (Direct a) -> Update (Direct a) -> IO ()
    fD (ModelF d t) (UpdateSet !a) = SE.triggerEvent t a
    fD _ _ = pure ()
    fN :: ZipTraverseF f => ModelF (Nested f) -> Update (Nested f) -> IO ()
    fN (ModelF d t) (UpdateSet !a) = SE.triggerEvent t a
    fN _ (UpdateKeep _) = pure ()
    fN (ModelF d t) (UpdatePropagate a) = do
      m <- SE.current $ SE.toBehavior d
      updateModel m a

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
