module ReactiveMarkup.Target.Gtk.Diagram where

import Control.Monad.IO.Class
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.GIGtk
import qualified GI.Gtk as Gtk
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk.Base
import ReactiveMarkup.Widget.Diagram
import qualified SimpleEvents as SE

instance MakeGtkRender (Diagram Gtk Cairo) c e => Render (Diagram Gtk Cairo) Gtk c where
  render (Diagram (GtkDynamic dia)) = MakeGtk $ do
    drawingArea <- Gtk.new Gtk.DrawingArea [#vexpand Gtk.:= True, #hexpand Gtk.:= True]
    


    -- _ <- SE.current (SE.toBehavior dia) >>= renderDiagram
    Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_ ctx w h ->
        SE.current (SE.toBehavior dia) >>= renderDiagram ctx 


    unreg <- liftIO $
      SE.reactimate (SE.toEvent dia) $
        SE.simpleEventHandler $ \_ -> Gtk.widgetQueueDraw drawingArea

    Gtk.on drawingArea #destroy $ SE.liftES unreg
    
    Gtk.toWidget drawingArea >>= setWidgetNow
    
    where
      renderDiagram ctx = defaultRender ctx True