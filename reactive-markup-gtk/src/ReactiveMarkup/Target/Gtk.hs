{-# LANGUAGE ImplicitParams #-}

module ReactiveMarkup.Target.Gtk 
  (Gtk, MakeGtk(..), runGtk) where

import Data.RHKT
import Control.Concurrent
import Control.Monad
import Data.Coerce (coerce)
import Data.Foldable (sequenceA_)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.IORef
import Data.Text as T
import Data.Void
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import GI.Pango.Functions ()
import ReactiveMarkup.App
import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Markup
import ReactiveMarkup.Widgets.Base
import ReactiveMarkup.Widgets.Eventful
import SimpleEvents (EventTrigger (triggerEvent))
import qualified SimpleEvents as SE
import System.Mem.StableName
import System.IO.Unsafe

import ReactiveMarkup.Target.Gtk.RenderInstances

onDifferentName :: s -> (s -> IO ()) -> IO (s -> IO ())
onDifferentName s f = do
  stableNameRef <- makeStableName s >>= newIORef
  pure $ \newS -> do
    newStableName <- makeStableName newS
    oldStableName <- readIORef stableNameRef
    when (oldStableName /= newStableName) $ do
      writeIORef stableNameRef oldStableName
      f newS
      
runGtk :: ZipTraverseF s => App s Gtk e -> IO ()
runGtk app = do
  model <- initiateModel (appInitialState app)

  let makeWidget = makeGtk (renderMarkup (appRender app $ modelToDynamic model)) $ \e -> do
        state <- modelToUpdateF model
        appHandleEvent app e state

  let activate app = do
        widget <- makeWidget
        window <-
          Gtk.new
            Gtk.ApplicationWindow
            [ #application Gtk.:= app,
              #title Gtk.:= "Hello",
              #child Gtk.:= widget
            ]

        #show window

  app <-
    Gtk.new
      Gtk.Application
      [ #applicationId Gtk.:= "haskell-gi.Gtk4.test",
        Gtk.On #activate (activate ?self)
      ]

  void $ #run app Nothing



data ModelStateF (f :: F) = ModelStateF (SE.Dynamic (ApplyNestedAndDirect f ModelStateF)) (SE.EventTrigger (ApplyNestedAndDirect f IdentityF))

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

modelToUpdateF :: ZipTraverseF x => x ModelStateF -> IO (x UpdateF)
modelToUpdateF = traverseF fD fN
  where
    fD :: ModelStateF (Direct a) -> IO (UpdateF (Direct a))
    fD (ModelStateF d t) = do
      v <- unsafeInterleaveIO $ SE.current $ SE.toBehavior d
      pure $ UpdateF v (SE.triggerEvent t)
    fN :: ZipTraverseF f => ModelStateF (Nested f) -> IO (UpdateF (Nested f))
    fN (ModelStateF d t) = do
      v <- unsafeInterleaveIO $ SE.current (SE.toBehavior d) >>= modelToUpdateF
      pure $ UpdateF v (SE.triggerEvent t)
