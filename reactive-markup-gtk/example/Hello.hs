import ReactiveMarkup.Target.Gtk
import ReactiveMarkup
import Data.Void
import Optics.Core


main :: IO ()
main = do
  runGtk app

renderGUI :: Markup Gtk Root Void
renderGUI = bold "Hello Reactive Markup"


app :: App Gtk EmptyF Void
app =
  App
    { appRender = \_ -> renderGUI,
      appHandleEvent = absurd,
      appInitialState = EmptyF,
      appName = "Hello Reactive Markup"
    }