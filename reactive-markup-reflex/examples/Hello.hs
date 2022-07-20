import ReactiveMarkup.Target.ReflexDom
import ReactiveMarkup
import Data.Void


main :: IO ()
main = do
  runReflexDomWarp app

renderGUI :: Markup RDom Root Void
renderGUI = bold "Hello Reactive Markup"

app :: App RDom EmptyF Void
app =
  App
    { appRender = \_ -> renderGUI,
      appHandleEvent = absurd,
      appInitialState = EmptyF,
      appName = "Hello Reactive Markup"
    }