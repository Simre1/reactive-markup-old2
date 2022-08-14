module ReactiveMarkup.Widget.Diagram where

import qualified Diagrams.Core as D
import ReactiveMarkup.Markup
import ReactiveMarkup.Context

-- newtype Diagram backend e = forall backend. DiagramBackend backend backend => Diagram (Dynamic t (D.Diagram backend))

newtype Diagram backend diaBackend e = Diagram (Dynamic backend (D.Diagram diaBackend))


-- class DiagramBackend backend backend | backend -> backend, backend -> backend

diagram :: forall diaBackend t c e. (Render (Diagram t diaBackend) t c) => Dynamic t (D.Diagram diaBackend) -> Markup t c e
diagram = markup . Diagram  