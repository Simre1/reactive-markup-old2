module ReactiveMarkup.Target.ReflexDom (runReflexDom, runReflexDomWarp, RDom) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class as T
import Data.Coerce
import ReactiveMarkup
import ReactiveMarkup.Target.ReflexDom.Base
import ReactiveMarkup.Target.ReflexDom.Container
import ReactiveMarkup.Target.ReflexDom.Diagram
import ReactiveMarkup.Target.ReflexDom.Paragraph
import ReactiveMarkup.Target.ReflexDom.Interactive
import ReactiveMarkup.Target.ReflexDom.ModelF
import ReactiveMarkup.Target.ReflexDom.State
import ReactiveMarkup.Target.ReflexDom.Styling
import Reflex.Dom

runReflexDom :: TransformFData s => App RDom s e -> Widget DomTimeline ()
runReflexDom app = do
  model <- initiateModel (appInitialState app)

  let handle e = do
        modelUpdate <- modelToUpdate model
        (_, modelUpdate') <- liftIO $ runModelM modelUpdate (appHandleEvent app e)
        updateModel model modelUpdate'

  let markup = appRender app (modelToDynamic model)

  let (ReflexWidget makeWidget) = renderMarkup markup

  makeWidget handle

runReflexDomWarp :: TransformFData s => App RDom s e -> IO ()
runReflexDomWarp app = mainWidget $ coerce $ runReflexDom app
