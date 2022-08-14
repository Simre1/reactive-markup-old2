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
    TransformFData,
    foldF2,
    mapF,
    traverseF,
  )
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import ReactiveMarkup.App
import ReactiveMarkup.Markup (Dynamic, renderMarkup)
import ReactiveMarkup.Target.Gtk.Base
import ReactiveMarkup.Target.Gtk.Container
import ReactiveMarkup.Target.Gtk.Paragraph
import ReactiveMarkup.Target.Gtk.Interactive
import ReactiveMarkup.Target.Gtk.ModelF
import ReactiveMarkup.Target.Gtk.State
import ReactiveMarkup.Target.Gtk.Styling
import ReactiveMarkup.Target.Gtk.Diagram
import ReactiveMarkup.Update
import qualified SimpleEvents as SE
import Data.Functor.Identity

-- onDifferentName :: s -> (s -> IO ()) -> IO (s -> IO ())
-- onDifferentName s f = do
--   stableNameRef <- makeStableName s >>= newIORef
--   pure $ \newS -> do
--     newStableName <- makeStableName newS
--     oldStableName <- readIORef stableNameRef
--     when (oldStableName /= newStableName) $ do
--       writeIORef stableNameRef oldStableName
--       f newS

runGtk :: TransformFData s => App Gtk s e -> IO ()
runGtk app = do
  model <- initiateModel (appInitialState app)

  (cleanUp, runCleanUp) <- makeCleanUp

  let activate gtkApp = do
        window <-
          Gtk.new
            Gtk.ApplicationWindow
            [ #application Gtk.:= gtkApp,
              #title Gtk.:= appName app
            ]
        let handle e = do
              modelUpdate <- modelToUpdate model
              (_, modelUpdate') <- runModelM modelUpdate (appHandleEvent app e)
              updateModel model (modelUpdate')
            setWidget = Gtk.windowSetChild window . Just


        runGtkContext (cleanUp, handle, setWidget) $
            makeGtk (renderMarkup (appRender app $ modelToDynamic model))
          
        maybeDisplay <- Gtk.get window #display
        styleProvider <- gtkStyleProvider
        maybe
          (pure ())
          (\display -> Gtk.styleContextAddProviderForDisplay display styleProvider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER))
          maybeDisplay
        
        #show window

  app <-
    Gtk.new
      Gtk.Application
      [ #applicationId Gtk.:= appName app,
        Gtk.On #activate (activate ?self)
      ]

  void $ #run app Nothing
  runCleanUp

-- modelToUpdateF :: TransformFData x => x ModelF -> IO (x (UpdateF Update))
-- modelToUpdateF = traverseF fD fN
--   where
--     fD :: ModelF (Direct a) -> IO (UpdateF Update (Direct a))
--     fD (ModelF d t) = do
--       v <- unsafeInterleaveIO $ SE.current $ SE.toBehavior d
--       pure $ UpdateF v (makeUpdate t)
--     fN :: TransformFData f => ModelF (Nested f) -> IO (UpdateF Update (Nested f))
--     fN (ModelF d t) = do
--       v <- unsafeInterleaveIO $ SE.current (SE.toBehavior d) >>= modelToUpdateF
--       pure $ UpdateF v (makeUpdate t)
