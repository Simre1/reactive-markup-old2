module ReactiveMarkup.Target.WidgetTree where


import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Widgets.Base
import ReactiveMarkup.Markup
import Data.Text
import ReactiveMarkup.Widgets.Eventful
import Data.Void (Void)

import ReactiveMarkup.Target.Html.Javascript

data Html

type instance RenderTarget Html c = B.Markup

type instance EventTarget Html = Void
