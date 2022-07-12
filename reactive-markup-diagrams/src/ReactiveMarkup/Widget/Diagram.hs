module ReactiveMarkup.Widget.Diagram where

import qualified Diagrams.Core as D
import ReactiveMarkup.Markup
import ReactiveMarkup.Context

-- newtype Diagram backend e = forall backend. DiagramBackend target backend => Diagram (Dynamic t (D.Diagram backend))

newtype Diagram target backend e = Diagram (Dynamic target (D.Diagram backend))


-- class DiagramBackend target backend | target -> backend, backend -> target

diagram :: forall backend t c e. (Render (Diagram t backend) t c) => Dynamic t (D.Diagram backend) -> Markup t c e
diagram = markup . Diagram  