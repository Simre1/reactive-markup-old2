module ReactiveMarkup.Target.Gtk.Interactive where

import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Foldable
import Data.Functor.Const
import qualified Data.GI.Base.Signals as GI
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Text as T
import Data.Void
import Data.Word
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import GI.Pango.Functions ()
import ReactiveMarkup.Context
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk.Base
import ReactiveMarkup.Widget
import qualified SimpleEvents as SE

instance MakeGtkRender (Button Gtk c) c e => Render (Button Gtk Inline) Gtk c where
  render (Button (ButtonOptions clickF) t) = MakeGtk $ do
    button <- Gtk.buttonNew
    handleEvent <- askHandleEvent
    localSetWidget (\w -> Gtk.buttonSetChild button (Just w)) $
      localHandleEvent absurd $
        makeGtk (pangoToWidget $ getConst $ renderMarkup t)
    maybe (pure ()) (\e -> void $ Gtk.onButtonClicked button $ handleEvent e) clickF
    Gtk.toWidget button >>= setWidgetNow

instance MakeGtkRender (TextField Gtk) c e => Render (TextField Gtk) Gtk c where
  render (TextField (TextFieldOptions handleActivate handleChange) value) = MakeGtk $ do
    handleEvent <- askHandleEvent
    entry <- Gtk.entryNew
    entryBuffer <- Gtk.entryGetBuffer entry
    currentValue <- liftIO $ SE.current $ SE.toBehavior $ SE.onlyTriggerOnChange $ coerce value
    Gtk.setEntryBufferText entryBuffer currentValue

    active <- liftIO $ newIORef True
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

    liftIO $ SE.reactimate (SE.toEvent $ coerce value) $ SE.simpleEventHandler update

    Gtk.toWidget entry >>= setWidgetNow

instance MakeGtkRender (HotKey Gtk c) c e => Render (HotKey Gtk c) Gtk c where
  render (HotKey f child) = MakeGtk $ do
    eventController <- Gtk.eventControllerKeyNew
    handleEvent <- askHandleEvent
    handler <- Gtk.onEventControllerKeyKeyPressed eventController (mapCallback handleEvent f)
    setWidget <- askSetWidget
    let newSetWidget w = do
          Gtk.on w #destroy $ GI.disconnectSignalHandler eventController handler
          Gtk.widgetAddController w eventController
          setWidget w
    localSetWidget newSetWidget $ makeGtk $ renderMarkup child
    where
      mapCallback :: forall e. (e -> IO ()) -> (Key -> [Modifier] -> Maybe e) -> Word32 -> Word32 -> [Gdk.ModifierType] -> IO Bool
      mapCallback handleEvent f gtkKeyval gtkKeycode gtkModifiers = do
        let modifiers = mapMaybe mapModifier gtkModifiers
         in case mapKey gtkKeyval >>= \k -> f k modifiers of
              Just e -> handleEvent e *> pure True
              Nothing -> pure False
      mapModifier :: Gdk.ModifierType -> Maybe Modifier
      mapModifier Gdk.ModifierTypeControlMask = Just ModControl
      mapModifier Gdk.ModifierTypeAltMask = Just ModAlt
      mapModifier Gdk.ModifierTypeShiftMask = Just ModShift
      mapModifier Gdk.ModifierTypeSuperMask = Just ModSuper
      mapModifier _ = Nothing
      mapKey :: Word32 -> Maybe Key
      mapKey gtkKey = case gtkKey of
        Gdk.KEY_q -> Just KeyQ
        Gdk.KEY_Q -> Just KeyQ
        Gdk.KEY_w -> Just KeyW
        Gdk.KEY_W -> Just KeyW
        Gdk.KEY_e -> Just KeyE
        Gdk.KEY_E -> Just KeyE
        Gdk.KEY_r -> Just KeyR
        Gdk.KEY_R -> Just KeyR
        Gdk.KEY_t -> Just KeyT
        Gdk.KEY_T -> Just KeyT
        Gdk.KEY_z -> Just KeyZ
        Gdk.KEY_Z -> Just KeyZ
        Gdk.KEY_u -> Just KeyU
        Gdk.KEY_U -> Just KeyU
        Gdk.KEY_i -> Just KeyI
        Gdk.KEY_I -> Just KeyI
        Gdk.KEY_o -> Just KeyO
        Gdk.KEY_O -> Just KeyO
        Gdk.KEY_p -> Just KeyP
        Gdk.KEY_P -> Just KeyP
        Gdk.KEY_a -> Just KeyA
        Gdk.KEY_A -> Just KeyA
        Gdk.KEY_s -> Just KeyS
        Gdk.KEY_S -> Just KeyS
        Gdk.KEY_d -> Just KeyD
        Gdk.KEY_D -> Just KeyD
        Gdk.KEY_f -> Just KeyF
        Gdk.KEY_F -> Just KeyF
        Gdk.KEY_j -> Just KeyJ
        Gdk.KEY_J -> Just KeyJ
        Gdk.KEY_k -> Just KeyK
        Gdk.KEY_K -> Just KeyK
        Gdk.KEY_l -> Just KeyL
        Gdk.KEY_L -> Just KeyL
        Gdk.KEY_y -> Just KeyY
        Gdk.KEY_Y -> Just KeyY
        Gdk.KEY_x -> Just KeyX
        Gdk.KEY_X -> Just KeyX
        Gdk.KEY_c -> Just KeyC
        Gdk.KEY_C -> Just KeyC
        Gdk.KEY_v -> Just KeyV
        Gdk.KEY_V -> Just KeyV
        Gdk.KEY_b -> Just KeyB
        Gdk.KEY_B -> Just KeyB
        Gdk.KEY_n -> Just KeyN
        Gdk.KEY_N -> Just KeyN
        Gdk.KEY_m -> Just KeyM
        Gdk.KEY_M -> Just KeyM
        Gdk.KEY_Down -> Just KeyDown
        Gdk.KEY_Up -> Just KeyUp
        Gdk.KEY_Left -> Just KeyLeft
        Gdk.KEY_Right -> Just KeyRight
        Gdk.KEY_ISO_Enter -> Just KeyEnter
        Gdk.KEY_space -> Just KeySpace
        _ -> Nothing
