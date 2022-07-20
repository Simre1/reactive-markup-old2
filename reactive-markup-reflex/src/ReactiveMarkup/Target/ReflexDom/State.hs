module ReactiveMarkup.Target.ReflexDom.State where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor (void)
import Data.Functor.Identity
import ReactiveMarkup
  ( DynamicMarkup (..),
    LocalState (..),
    Render (..),
    runModelM,
  )
import ReactiveMarkup.Target.ReflexDom.Base
import ReactiveMarkup.Target.ReflexDom.ModelF
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Widget as W

instance Render (DynamicMarkup s RDom c) RDom c where
  render (DynamicMarkup dynamicState makeMarkup) = ReflexWidget $ \t -> do
    let (ReflexDomDynamic d) = dynamicState

    dyn_ (renderReflexWidget t . makeMarkup <$> d)

instance Render (LocalState s RDom c) RDom c where
  render (LocalState update initial makeMarkup) = ReflexWidget $ \handleOuterEvent -> do
    chan <- askEvents
    model <- initiateModel initial

    let handleInnerEvent innerEvent = do
          modelUpdate <- modelToUpdate model
          let (outerEvent, modelUpdate') = runIdentity $ runModelM modelUpdate (update innerEvent)
          updateModel model modelUpdate'
          maybe (pure ()) handleOuterEvent outerEvent

    renderReflexWidget handleInnerEvent (makeMarkup (modelToDynamic model))