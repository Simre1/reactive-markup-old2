module ReactiveMarkup.Target.ReflexDom.ModelF where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Trans.Control
import Data.Dependent.Sum
import Data.Monoid (Ap (Ap, getAp))
import Data.RHKT
import ReactiveMarkup
import ReactiveMarkup.Target.ReflexDom.Base
import Reflex as R
import Reflex.Dom
import Reflex.Host.Class

data ModelF (f :: F) = ModelF (R.Dynamic DomTimeline (ApplyF f ModelF)) (Trigger (ApplyF f ID))

-- mkDynamic ::
--   forall m a.
--   ( MonadHold DomTimeline m,
--     MonadReflexCreateTrigger DomTimeline m,
--     MonadRef m,
--     MonadReflexHost DomTimeline m,
--     Ref m ~ Ref IO
--   ) =>
--   a ->
--   m (R.Dynamic DomTimeline a, Trigger a)
-- mkDynamic a = do
--   (ev, trigger) <- newEventWithTriggerRef
--   dyn <- holdDyn a ev
--   pure (dyn, fireEventRef' trigger)
--   where
--     fireEventRef' :: Ref m (Maybe (EventTrigger DomTimeline a)) -> a -> m ()
--     fireEventRef' = fireEventRef

-- mkDynamic ::
--   (Monad m, MonadReflexCreateTrigger DomTimeline m, MonadRef m, MonadHold DomTimeline m, Ref m ~ Ref IO) =>
--   a ->
--   m (R.Dynamic DomTimeline a, Trigger a)
-- mkDynamic a = do
--   (ev, trigger) <- newEventWithTriggerRef
--   dyn <- holdDyn a ev
--   pure (dyn, liftIO . runDomHost . fireEventRef trigger)

mkDynamic ::
  (Monad m, TriggerEvent DomTimeline m, MonadHold DomTimeline m) =>
  a ->
  m (R.Dynamic DomTimeline a, Trigger a)
mkDynamic a = do
  (ev, trigger) <- newTriggerEvent
  dyn <- holdDyn a ev
  pure (dyn, liftIO . trigger)

initiateModel :: (TransformFData x) => x ID -> Widget DomTimeline (x ModelF)
initiateModel x = do
  chan <- Reflex.Dom.askEvents
  initiateModel' chan x

initiateModel' ::
  forall x m.
  ( TransformFData x,
    MonadHold DomTimeline m,
    MonadReflexCreateTrigger DomTimeline m,
    TriggerEvent DomTimeline m
  ) =>
  Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation] ->
  x ID ->
  m (x ModelF)
initiateModel' chan = traverseF fD fN
  where
    fD :: forall a. ID (Direct a) -> m (ModelF (Direct a))
    fD (ID a) = do
      (d, t) <- mkDynamic a
      pure $ ModelF d t
    fN :: forall a. TransformFData a => ID (Nested a) -> m (ModelF (Nested a))
    fN (ID a) = do
      m <- initiateModel' chan a
      (d, t) <- mkDynamic m
      pure $
        ModelF d $ \a ->
          runTriggerEventT
            (initiateModel' chan a)
            chan
            >>= t

modelToDynamic :: TransformFData x => x ModelF -> x (FunctorF (ReactiveMarkup.Target.ReflexDom.Base.Dynamic RDom))
modelToDynamic = mapF (\(ModelF a _) -> FunctorF (ReflexDomDynamic a)) (\(ModelF a _) -> FunctorF $ ReflexDomDynamic $ modelToDynamic <$> a)

modelToUpdate :: forall x m. (TransformFData x) => x ModelF -> Performable (Widget DomTimeline) (x Update)
modelToUpdate = traverseF fD fN
  where
    fD :: ModelF (Direct a) -> Performable (Widget DomTimeline) (Update (Direct a))
    fD (ModelF d t) = fmap UpdateKeep $ sample $ current d
    fN :: TransformFData f => ModelF (Nested f) -> Performable (Widget DomTimeline) (Update (Nested f))
    fN (ModelF d t) = do
      a <- sample $ current d
      UpdateKeep <$> modelToUpdate a

updateModel ::
  forall x.
  TransformFData x =>
  x ModelF ->
  x Update ->
  Performable (Widget DomTimeline) ()
updateModel m u = getAp $ foldF2 fD fN m u
  where
    fD :: ModelF (Direct a) -> Update (Direct a) -> Ap (Performable (Widget DomTimeline)) ()
    fD (ModelF d t) (UpdateSet !a) = Ap $ t a
    fD _ _ = pure ()
    fN :: TransformFData f => ModelF (Nested f) -> Update (Nested f) -> Ap (Performable (Widget DomTimeline)) ()
    fN (ModelF d t) (UpdateSet !a) = Ap $ t a
    fN _ (UpdateKeep _) = pure ()
    fN (ModelF d t) (UpdatePropagate a) = Ap $ do
      m <- sample $ current d
      updateModel m a
