module ReactiveMarkup.Widget where

class Render widget context target where
  render :: widget context target -> target context

type Widget = * -> (* -> *) -> *

data Markup target context = forall widget. Render widget target context => Markup (widget target context)

markup :: (Render widget target context) => widget target context -> Markup target context
markup = Markup

runMarkup :: Markup context target -> target context
runMarkup (Markup elem) = render elem
