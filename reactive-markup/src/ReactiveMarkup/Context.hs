module ReactiveMarkup.Context where

import Data.Coerce
import Optics.Core
import ReactiveMarkup.Markup

data Root

data Common

data Paragraph

data Single (widget :: * -> *)

type instance RenderTarget backend (Single widget) = widget

instance c ~ widget => Render widget backend (Single c) where
  render widget = widget

widget ::
  Render widget2 backend context =>
  Iso
    (Markup backend (Single widget1) event1)
    (Markup backend context event2)
    (widget1 event1)
    (widget2 event2)
widget = iso renderMarkup wrapMarkup

modifyWidget :: Render widget2 backend context => (widget1 event1 -> widget2 event2) -> Markup backend (Single widget1) event1 -> Markup backend context event2
modifyWidget f = wrapMarkup . f . renderMarkup

($=) ::
  Render widget2 backend context =>
  Markup backend (Single widget1) event1 ->
  (widget1 event1 -> widget2 event2) ->
  Markup backend context event2
($=) = flip modifyWidget

infix 3 $=
