module ReactiveMarkup.Target.Threepenny where

import ReactiveMarkup.Markup
import ReactiveMarkup.Widgets.Base
import ReactiveMarkup.Widgets.Eventful

import Graphics.UI.Threepenny as UI

import Data.Void
import ReactiveMarkup.Contexts.Base
import Data.Functor (($>))
import Data.Function ((&))
import Data.Text (unpack)
import Data.List
data Threepenny

type instance RenderTarget Threepenny c = ThreepennyElement

type instance EventTarget Threepenny = UI ()

type instance Function Threepenny = (->)

startThreepenny :: (e -> EventTarget Threepenny) -> Markup Threepenny Root e -> IO ()
startThreepenny handle markup = do
  startGUI config $ \w -> getBody w #+ [runElement $ renderMarkup handle markup] $> ()
    
  where config = defaultConfig {jsCustomHTML = Just "template.html", jsStatic = Just "static", jsWindowReloadOnDisconnect=True}
 
runElement :: ThreepennyElement -> UI Element
runElement (ThreepennyElement e c) = if null c then e else e # set class_ (unwords $ reverse c)

createElement :: UI Element -> ThreepennyElement
createElement e = ThreepennyElement e []

modifyElement :: (UI Element -> UI Element) -> ThreepennyElement -> ThreepennyElement
modifyElement f (ThreepennyElement e c) = ThreepennyElement (f e) c

appendClass :: String -> ThreepennyElement -> ThreepennyElement
appendClass c (ThreepennyElement e cs) = ThreepennyElement e (c:cs)

data ThreepennyElement = ThreepennyElement (UI Element) [String]

instance Render Words Threepenny c where
  render _ (Words t) = createElement $ UI.string (unpack t)

instance Render (Emphasis Threepenny Inline) Threepenny c where
  render handle (Emphasis child) = appendClass "font-italic" $ renderMarkup handle child

instance Render (Bold Threepenny Inline) Threepenny c where
  render handle (Bold child) = appendClass "font-bold" $ renderMarkup handle child

instance Render (Combine Threepenny Inline) Threepenny c where
  render handle (Combine a b) = do
    let e1 = runElement $ renderMarkup handle a
        e2 = runElement $ renderMarkup handle b
    createElement $ UI.span #+ [e1, e2]

instance Render (List Threepenny c) Threepenny c where
  render handle (List ms) = foldMap (renderMarkup handle) ms

-- instance Render (Button Threepenny Inline) Threepenny Block where
--   render f (Button m) = undefined
