{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module ReactiveMarkup.Target.Gtk (Gtk, MakeGtk (..), runGtk) where

import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.RHKT
  ( ApplyF,
    F (..),
    FunctorF (FunctorF),
    IdentityF (..),
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
    ModelState (ModelState),
    Update (UpdateKeep, UpdatePropagate, UpdateSet),
    UpdateF (..),
    appName,
    getInternalModelState,
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
        updatedModelState <- appHandleEvent app e (ModelState state)
        updateModel model (getInternalModelState updatedModelState)

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

data ModelStateF (f :: F) = ModelStateF (SE.Dynamic (ApplyF f ModelStateF)) (SE.EventTrigger (ApplyF f IdentityF))

initiateModel :: forall x. ZipTraverseF x => x IdentityF -> IO (x ModelStateF)
initiateModel = traverseF fD fN
  where
    fD :: forall a. IdentityF (Direct a) -> IO (ModelStateF (Direct a))
    fD (IdentityF a) = do
      (d, t) <- SE.newDynamic a
      pure $ ModelStateF d t
    fN :: forall a. ZipTraverseF a => IdentityF (Nested a) -> IO (ModelStateF (Nested a))
    fN (IdentityF a) = do
      m <- initiateModel a
      (d, t) <- SE.newDynamic m
      pure $ ModelStateF d $ SE.mapEventTrigger initiateModel t

modelToDynamic :: ZipTraverseF x => x ModelStateF -> x (FunctorF (Dynamic Gtk))
modelToDynamic = mapF (\(ModelStateF a _) -> FunctorF (GtkDynamic a)) (\(ModelStateF a _) -> FunctorF $ GtkDynamic $ modelToDynamic <$> a)

modelToUpdate :: ZipTraverseF x => x ModelStateF -> IO (x Update)
modelToUpdate = traverseF fD fN
  where
    fD :: ModelStateF (Direct a) -> IO (Update (Direct a))
    fD (ModelStateF d t) = unsafeInterleaveIO $ fmap UpdateKeep $ SE.current $ SE.toBehavior d
    fN :: ZipTraverseF f => ModelStateF (Nested f) -> IO (Update (Nested f))
    fN (ModelStateF d t) = unsafeInterleaveIO $ do
      a <- SE.current $ SE.toBehavior d
      UpdateKeep <$> modelToUpdate a

updateModel :: ZipTraverseF x => x ModelStateF -> x Update -> IO ()
updateModel = foldF2 fD fN
  where
    fD :: ModelStateF (Direct a) -> Update (Direct a) -> IO ()
    fD (ModelStateF d t) (UpdateSet !a) = SE.triggerEvent t a
    fD _ _ = pure ()
    fN :: ZipTraverseF f => ModelStateF (Nested f) -> Update (Nested f) -> IO ()
    fN (ModelStateF d t) (UpdateSet !a) = SE.triggerEvent t a
    fN _ (UpdateKeep _) = pure ()
    fN (ModelStateF d t) (UpdatePropagate a) = do
      m <- SE.current $ SE.toBehavior d
      updateModel m a

-- modelToUpdateF :: ZipTraverseF x => x ModelStateF -> IO (x (UpdateF Update))
-- modelToUpdateF = traverseF fD fN
--   where
--     fD :: ModelStateF (Direct a) -> IO (UpdateF Update (Direct a))
--     fD (ModelStateF d t) = do
--       v <- unsafeInterleaveIO $ SE.current $ SE.toBehavior d
--       pure $ UpdateF v (makeUpdate t)
--     fN :: ZipTraverseF f => ModelStateF (Nested f) -> IO (UpdateF Update (Nested f))
--     fN (ModelStateF d t) = do
--       v <- unsafeInterleaveIO $ SE.current (SE.toBehavior d) >>= modelToUpdateF
--       pure $ UpdateF v (makeUpdate t)
