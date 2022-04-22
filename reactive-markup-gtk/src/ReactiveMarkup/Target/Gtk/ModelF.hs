module ReactiveMarkup.Target.Gtk.ModelF where

import ReactiveMarkup.Markup
import ReactiveMarkup.Update
import qualified SimpleEvents as SE
import Data.RHKT
import ReactiveMarkup.Target.Gtk.Base
import GHC.IO (unsafeInterleaveIO)

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