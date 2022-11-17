module ReactiveMarkup.Target.ReflexDom.Interactive where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor (void, ($>))
import Data.Void
import ReactiveMarkup
import ReactiveMarkup.Target.ReflexDom.Base
import Reflex.Dom (DomBuilder (element), EventName (Click, Keypress), HasDomEvent (domEvent), MonadSample (sample), Reflex (current, updated), runDomHost, runSpiderHost, (=:))
import qualified Reflex.Dom.Builder.Class as W
import Reflex.Dom.Widget as W
import Reflex.Host.Class

instance Render (Button RDom Paragraph) RDom Common where
  render (Button clickF m) = ReflexWidget $ \t -> do
    (buttonElement, _) <- element "button" def $ renderReflexWidget absurd m
    let clickEvent = domEvent Click buttonElement
    maybe
      (pure ())
      (\eventValue -> void $ reactimate clickEvent (\_ -> t eventValue))
      clickF
    pure ()

instance Render (TextField RDom) RDom Common where
  render (TextField handleActivate handleChange (ReflexDomDynamic dValue)) = ReflexWidget $ \t -> do
    val <- sample $ current dValue
    let event = updated dValue
    textfield <- W.inputElement def {W._inputElementConfig_initialValue = val, W._inputElementConfig_setValue = Just event}

    let changeEvent = W._inputElement_input textfield

    let activateEvent = domEvent Keypress textfield

    maybe (pure ()) (\f -> reactimate changeEvent (t . f)) handleChange

    maybe
        (pure ())
        ( \f ->
            void $
              reactimate
                activateEvent
                ( \w -> when (w == 13) $ do
                    val <- sample $ current dValue
                    t $ f val
                )
        )
        handleActivate

    -- (buttonElement, _) <- element "button" def $ renderReflexWidget absurd m
    -- let clickEvent = domEvent Click buttonElement
    -- maybe
    --   (pure ())
    --   (\eventValue -> void $ liftIO $ reactimate clickEvent (\_ -> t eventValue))
    --   clickF
    pure ()

-- instance MakeGtkRender (TextField Gtk) c e => Render (TextField Gtk) Gtk c where
--   render (TextField (TextFieldOptions handleActivate handleChange) value) = MakeGtk $ do
--     handleEvent <- askHandleEvent
--     entry <- Gtk.entryNew
--     entryBuffer <- Gtk.entryGetBuffer entry
--     currentValue <- liftIO $ SE.current $ SE.toBehavior $ SE.onlyTriggerOnChange $ coerce value
--     Gtk.setEntryBufferText entryBuffer currentValue

--     active <- liftIO $ newIORef True
--     let protect a = do
--           whenM (readIORef active) $ do
--             a

--     sequenceA_ $ (\handle -> Gtk.after entry #changed $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle) <$> handleChange

--     sequenceA_ $ (\handle -> Gtk.onEntryActivate entry $ protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle) <$> handleActivate
--     -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferDeletedText entryBuffer $ \_ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange
--     -- sequenceA_ $ (\handle -> Gtk.afterEntryBufferInsertedText entryBuffer $ \_ _ _ -> protect $ Gtk.entryBufferGetText entryBuffer >>= handleEvent . handle . TextFieldEvent) <$> handleChange

--     let update = \newText -> do
--           writeIORef active False
--           p <- Gtk.get entry #cursorPosition
--           Gtk.setEntryBufferText entryBuffer newText
--           Gtk.editableSetPosition entry p
--           writeIORef active True

--     liftIO $ SE.reactimate (SE.toEvent $ coerce value) $ SE.simpleEventHandler update

--     Gtk.toWidget entry >>= setWidgetNow