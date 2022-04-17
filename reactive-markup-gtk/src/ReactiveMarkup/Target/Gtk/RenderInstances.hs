{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}

module ReactiveMarkup.Target.Gtk.RenderInstances where

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
import ReactiveMarkup.Context
import ReactiveMarkup.Markup
import ReactiveMarkup.Widget
import qualified SimpleEvents as SE

data Gtk

type instance RenderTarget Gtk Inline = Const Text

newtype MakeGtk e = MakeGtk {makeGtk :: (e -> IO ()) -> IO Gtk.Widget}

type instance RenderTarget Gtk Block = MakeGtk

type instance RenderTarget Gtk Root = MakeGtk

newtype instance Dynamic Gtk a = GtkDynamic (SE.Dynamic a) deriving (Functor, Applicative, Monad)

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

instance Render (Italic Gtk Inline) Gtk Inline where
  render (Italic m) = Const $ "<i>" <> getConst (renderMarkup m) <> "</i>"

instance Render (Italic Gtk Inline) Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render (Italic Gtk Inline) Gtk Root where
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

instance Render (Column Gtk Block) Gtk Block where
  render (Column ms) = MakeGtk $ \handle -> do
    box <- Gtk.boxNew Gtk.OrientationVertical 0
    forM_ ms $ \m -> do
      child <- makeGtk (renderMarkup m) handle
      Gtk.boxAppend box child
    Gtk.toWidget box

instance Render (Row Gtk Block) Gtk Block where
  render (Row ms) = MakeGtk $ \handle -> do
    box <- Gtk.boxNew Gtk.OrientationHorizontal 0
    forM_ ms $ \m -> do
      child <- makeGtk (renderMarkup m) handle
      Gtk.boxAppend box child
    Gtk.toWidget box

instance MakeGtk ~ RenderTarget c t => Render (Map c t) c t where
  render (Map f m) = MakeGtk $ \handle -> makeGtk (renderMarkup m) (handle . f)

instance Render (Column Gtk Block) Gtk Root where
  render b = render @_ @Gtk @Block b

instance Render (Row Gtk Block) Gtk Root where
  render b = render @_ @Gtk @Block b

instance Render (Button Gtk Inline) Gtk Block where
  render (Button t (ButtonOptions clickF)) = MakeGtk $ \handle -> do
    label <- makeGtk (pangoToWidget $ getConst $ renderMarkup t) absurd
    button <- Gtk.buttonNew
    Gtk.buttonSetChild button (Just label)
    maybe mempty (\e -> void $ Gtk.onButtonClicked button $ handle e) clickF
    Gtk.toWidget button

instance (RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (LocalState s Gtk c) Gtk c, RenderTarget Gtk c e ~ MakeGtk e) => Render (LocalState s Gtk c) Gtk c where
  render (LocalState update initial makeMarkup) = MakeGtk $ \handleOuterEvent -> do
    (dynamicState, updateState) <- SE.newDynamic initial
    let handleInnerEvent innerEvent = do
          state <- SE.current $ SE.toBehavior dynamicState
          let (changedState, outerEvent) = update state innerEvent
          maybe (pure ()) (SE.triggerEvent updateState) changedState
          maybe (pure ()) handleOuterEvent outerEvent
    makeGtk (renderMarkup $ makeMarkup (coerce dynamicState)) handleInnerEvent

instance (RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (Counter Gtk c) Gtk c, RenderTarget Gtk c e ~ MakeGtk e) => Render (Counter Gtk c) Gtk c where
  render (Counter intervall f) = MakeGtk $ \handle -> do
    (d, t) <- SE.newDynamic 0
    GLib.timeoutAdd GLib.PRIORITY_DEFAULT (round (intervall * 1000)) $ do
      SE.current (SE.toBehavior d) >>= SE.triggerEvent t . succ
      pure True
    -- Gtk.on widget #destroy $ killThread thread
    makeGtk (renderMarkup (f $ GtkDynamic d)) handle

instance (RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (DynamicMarkup s Gtk c) Gtk c, RenderTarget Gtk c e ~ MakeGtk e) => Render (DynamicMarkup s Gtk c) Gtk c where
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
        generateWidget state = do
          !s <- pure state
          makeGtk (renderMarkup (makeMarkup s)) handleEvent

    let handler newState = do
          w <- generateWidget newState
          setWidget w



    unregisterWidgetUpdate <-
      SE.reactimate (SE.toEvent $ coerce dynamicState) $ SE.simpleEventHandler handler
    generateWidget state >>= setWidget
    widget <- Gtk.toWidget frame
    Gtk.on widget #destroy (SE.liftES unregisterWidgetUpdate)
    pure widget

instance (RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (TextField Gtk) Gtk c, RenderTarget Gtk c e ~ MakeGtk e) => Render (TextField Gtk) Gtk c where
  render (TextField (TextFieldOptions value handleActivate handleChange)) = MakeGtk $ \handleEvent -> do
    entry <- Gtk.entryNew
    entryBuffer <- Gtk.entryGetBuffer entry
    currentValue <- SE.current $ SE.toBehavior $ SE.onlyTriggerOnChange $ coerce value
    Gtk.setEntryBufferText entryBuffer currentValue

    active <- newIORef True
    let protect a = do
          whenM (readIORef active) $ do
            a

    sequenceA_ $ (\handle -> Gtk.after entry #changed $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle) <$> handleChange

    sequenceA_ $ (\handle -> Gtk.onEntryActivate entry $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle) <$> handleActivate
    -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferDeletedText entryBuffer $ \_ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange
    -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferInsertedText entryBuffer $ \_ _ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange

    let update = \newText -> do
          writeIORef active False
          p <- Gtk.get entry #cursorPosition
          Gtk.setEntryBufferText entryBuffer newText
          Gtk.editableSetPosition entry p
          writeIORef active True

    SE.reactimate (SE.toEvent $ coerce value) $ SE.simpleEventHandler update

    Gtk.toWidget entry

instance (RenderErrorOnEqual (RenderTarget Gtk c e) (MakeGtk e) (MapEventIO Gtk c) Gtk c, RenderTarget Gtk c e ~ MakeGtk e) => Render (MapEventIO Gtk c) Gtk c where
  render (MapEventIO f m) = MakeGtk $ \handle ->
    makeGtk (renderMarkup m) (\e -> f e >>= maybe (pure ()) handle)
    

whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = do
  b <- c
  when b a

