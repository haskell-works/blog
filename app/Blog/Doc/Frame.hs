module Blog.Doc.Frame where

import Diagrams.Backend.SVG
import Diagrams.Prelude

enframe :: Diagram B -> Diagram B
enframe d = rect 80 60 <> d

