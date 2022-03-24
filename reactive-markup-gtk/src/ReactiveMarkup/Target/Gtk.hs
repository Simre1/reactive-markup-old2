{-# LANGUAGE ImplicitParams #-}

module ReactiveMarkup.Target.Gtk where

import Control.Monad
import Data.IORef
import Data.Text as T
import Data.Void
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import GI.Pango.Functions ()
import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Markup
import ReactiveMarkup.Widgets.Base
import ReactiveMarkup.Widgets.Eventful
import qualified SimpleEvents as SE
import ReactiveMarkup.App
import Data.Coerce (coerce)
import Data.Functor.Const
import Data.Foldable (sequenceA_)
import System.Mem.StableName
import Data.Functor.Identity
import Data.Functor.Product
import SimpleEvents (EventTrigger(triggerEvent))

data Gtk

type instance RenderTarget Gtk Inline = Const Text

newtype MakeGtk e = MakeGtk {makeGtk :: (e -> IO ()) -> IO Gtk.Widget}

type instance RenderTarget Gtk Block = MakeGtk

type instance RenderTarget Gtk Root = MakeGtk

newtype instance Dynamic Gtk a = GtkDynamic (SE.Dynamic a) deriving (Functor, Applicative)

pangoToWidget :: Text -> MakeGtk e
pangoToWidget t = MakeGtk . const $ do
  label <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label t
  Gtk.toWidget label

renderInline :: Render w Gtk Inline => w e -> Text
renderInline = getConst . render @_ @Gtk @Inline

instance RenderTarget Gtk c ~ MakeGtk => Render (FilterEvents Gtk c) Gtk c where
  render (FilterEvents f m) = MakeGtk $ \handle -> makeGtk (renderMarkup m) (newF handle)
    where
      newF h e = maybe (pure ()) h (f e)

instance Render Words Gtk Inline where
  render (Words t) = Const t

instance Render Words Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render Words Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Emphasis Gtk Inline) Gtk Inline where
  render (Emphasis m) = Const $ "<i>" <> getConst (renderMarkup m) <> "</i>"

instance Render (Emphasis Gtk Inline) Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render (Emphasis Gtk Inline) Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Bold Gtk Inline) Gtk Inline where
  render (Bold m) = Const $ "<b>" <> getConst (renderMarkup m) <> "</b>"

instance Render (Bold Gtk Inline) Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render (Bold Gtk Inline) Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Combine Gtk Inline) Gtk Inline where
  render (Combine a b) = Const $ getConst (renderMarkup a) <> getConst (renderMarkup b)

instance Render (Combine Gtk Inline) Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render (Combine Gtk Inline) Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Lift Gtk Inline) Gtk Root where
  render (Lift m) = pangoToWidget $ getConst $ renderMarkup m

instance Render (Lift Gtk Block) Gtk Root where
  render (Lift m) = renderMarkup m

instance Render (Lift Gtk Inline) Gtk Block where
  render (Lift m) = pangoToWidget $ getConst $ renderMarkup m

instance Render (Blocks Gtk Block) Gtk Block where
  render (Blocks ms) = MakeGtk $ \handle -> do
    box <- Gtk.boxNew Gtk.OrientationVertical 0
    forM_ ms $ \m -> do
      child <- makeGtk (renderMarkup m) handle
      Gtk.boxAppend box child
    Gtk.toWidget box

instance MakeGtk ~ RenderTarget c t => Render (Map c t) c t where
  render (Map f m) = MakeGtk $ \handle -> makeGtk (renderMarkup m) (handle . f)

instance Render (Blocks Gtk Block) Gtk Root where
  render b = render @_ @Gtk @Block b

instance Render (Button Gtk Inline) Gtk Block where
  render (Button t) = MakeGtk $ \handle -> do
    label <- makeGtk (pangoToWidget $ getConst $ renderMarkup t) absurd
    button <- Gtk.buttonNew
    Gtk.buttonSetChild button (Just label)
    Gtk.onButtonClicked button $ handle ButtonClick
    Gtk.toWidget button

instance RenderTarget Gtk c e ~ MakeGtk e => Render (LocalState s Gtk c) Gtk c where
  render (LocalState update initial makeMarkup) = MakeGtk $ \handleOuterEvent -> do
    (dynamicState, updateState) <- SE.newDynamic initial
    let handleInnerEvent innerEvent = do
          state <- SE.current $ SE.toBehavior dynamicState
          let (changedState, outerEvent) = update state innerEvent
          maybe (pure ()) (SE.triggerEvent updateState) changedState
          maybe (pure ()) handleOuterEvent outerEvent
    makeGtk (renderMarkup $ makeMarkup (coerce dynamicState)) handleInnerEvent

instance RenderTarget Gtk c e ~ MakeGtk e => Render (DynamicMarkup s Gtk c) Gtk c where
  render (DynamicMarkup dynamicState makeMarkup) = MakeGtk $ \handleEvent -> do
    frame <- Gtk.boxNew Gtk.OrientationVertical 0

    -- Gtk.frameSetShadowType frame Gtk.ShadowTypeNone
    state <- SE.current $ SE.toBehavior (coerce dynamicState)

    cleanUpRef <- newIORef (pure ())
    let setWidget widget = do
          cleanUp <- join $ readIORef cleanUpRef
          writeIORef cleanUpRef (Gtk.boxRemove frame widget)
          Gtk.boxAppend frame widget
        -- #showAll widget
        generateWidget state =
          makeGtk (renderMarkup (makeMarkup state)) handleEvent


    let handler = \newState -> generateWidget newState >>= setWidget

    unregisterWidgetUpdate <-
      SE.reactimate (SE.toEvent $ coerce dynamicState) $ SE.simpleEventHandler handler
    generateWidget state >>= setWidget
    widget <- Gtk.toWidget frame
    Gtk.on widget #destroy (SE.liftES unregisterWidgetUpdate)
    pure widget

instance RenderTarget Gtk c e ~ MakeGtk e => Render (TextField Gtk) Gtk c where
  render (TextField value handleActivate handleChange) = MakeGtk $ \handleEvent -> do
    entry <- Gtk.entryNew
    entryBuffer <- Gtk.entryGetBuffer entry
    currentValue <- SE.current $ SE.toBehavior $ SE.onlyTriggerOnChange $ coerce value
    Gtk.setEntryBufferText entryBuffer currentValue

    active <- newIORef True
    let protect a = do
          whenM (readIORef active) $ do
            a

    
    sequenceA_ $ (\handle -> Gtk.after entry #changed $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange

    sequenceA_ $ (\handle -> Gtk.onEntryActivate entry $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleActivate
    -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferDeletedText entryBuffer $ \_ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange
    -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferInsertedText entryBuffer $ \_ _ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange

    let update = \newText -> do
          print newText
          writeIORef active False
          p <- Gtk.get entry #cursorPosition
          Gtk.setEntryBufferText entryBuffer newText
          Gtk.editableSetPosition entry p
          writeIORef active True

    SE.reactimate (SE.toEvent $ coerce value) $ SE.simpleEventHandler update

    Gtk.toWidget entry


onDifferentName :: s -> (s -> IO ()) -> IO (s -> IO ())
onDifferentName s f = do
  stableNameRef <- makeStableName s >>= newIORef
  pure $ \newS -> do
          newStableName <- makeStableName newS
          oldStableName <- readIORef stableNameRef
          when (oldStableName /= newStableName) $ do
            writeIORef stableNameRef oldStableName
            f newS

whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = do
  b <- c
  when b a


runGtk :: App s Gtk e -> IO ()
runGtk app = do
  (state, trigger) <- SE.newDynamic (appInitialState app)


  let makeWidget = makeGtk (renderMarkup (appRender app $ coerce state)) $ \e -> do
        s <- SE.current (SE.toBehavior state)
        s' <- appHandleEvent app e s
        SE.triggerEvent trigger s'

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


data Trigger a = forall b. Trigger (a -> b) (EventTrigger b)
data ModelState a = ModelState (SE.Dynamic a) (Trigger a) (Update a)

initiateModel :: ZipTraverseF m => m Identity -> IO (m ModelState)
initiateModel = traverseF $ \f (Identity a) -> do
      (d,t) <- f a >>= SE.newDynamic
      val <- f a
      pure $ ModelState d (Trigger undefined t) (Update val (triggerEvent t))


makeUpdateModel :: ZipTraverseF x => x ModelState -> IO (x Update)
makeUpdateModel = traverseF $ \f (ModelState a b _) -> do
  val <- SE.current (SE.toBehavior a) >>= f
  pure $ Update val undefined

-- test :: Model (Product SE.Dynamic SE.EventTrigger) -> IO ()
-- test (Model a b) = do
--   SE.triggerEvent (rightProduct a) $ "hello"
--   newValues >>= SE.triggerEvent (rightProduct b)
--   where 
--     newValues :: IO [Product SE.Dynamic SE.EventTrigger Int]
--     newValues = pure . unwrap <$> (initiateModel (Wrap (Identity 5)))


-- update :: (m Set -> m Set) -> m (Product SE.Dynamic SE.EventTrigger) -> IO ()
-- update = undefined
--   where
--     combined :: m (Product Set (Product SE.Dynamic SE.EventTrigger))
--     combined = zip
