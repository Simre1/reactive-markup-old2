module ReactiveMarkup.Target.Html.Render where

import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Widgets.Base
import ReactiveMarkup.Markup
import Text.Blaze.Html5 hiding (Markup, Html)

import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as B
import Data.Text
import Formatting
import Control.Category
import Prelude hiding ((.), id)
import ReactiveMarkup.Widgets.Eventful
import Data.Void (Void)

import ReactiveMarkup.Target.Html.Javascript

data Html

type instance RenderTarget Html c = B.Markup

type instance EventTarget Html = Void

type instance Function Html = JSFunc

runHtml :: JSFunc e (EventTarget Html) -> Markup Html context e -> B.Markup 
runHtml f = html . renderMarkup f

makeAttr :: JSVal a -> AttributeValue
makeAttr (JSVal a) = toValue a

instance Render Words Html c where
  render _ (Words t) = toHtml t

instance Render (Emphasis Html Inline) Html c where
  render handle (Emphasis child) = i $ renderMarkup handle child

instance Render (Bold Html Inline) Html c where
  render handle (Bold child) = b $ renderMarkup handle child

instance Render (Combine Html Inline) Html c where
  render handle (Combine a b) = renderMarkup handle a <> renderMarkup handle b

instance Render (List Html c) Html c where
  render handle (List ms) = foldMap (renderMarkup handle) ms

instance Render (Button Html Inline) Html Block where
  render f (Button m) = B.button ! onclick (makeAttr $ jsApply f $ toJSVal ()) $ renderMarkup jsAbsurd m


