{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module Blog.Doc.Json.Text where

import Diagrams.Backend.SVG
import Diagrams.Prelude
import System.IO.Unsafe

import qualified Graphics.SVGFonts          as SF
import qualified Graphics.SVGFonts.ReadFont as SF

lin2 :: SF.PreparedFont Double
lin2 = unsafePerformIO SF.lin2
{-# NOINLINE lin2 #-}

text' :: Renderable (Path V2 Double) b => Double -> String -> QDiagram b V2 Double Any
text' d s = strokeP (SF.textSVG' (SF.TextOpts lin2 SF.INSIDE_H SF.KERN False d d) s)
          # lw none

example :: Diagram B
example = text' 1 "Hello" # fc blue ||| text' 1 "world" # fc green

highlightText :: String -> String -> [Char -> Diagram B -> Diagram B] -> Diagram B
highlightText s selection styles = hsep 0 (fmap char s) # center

char :: Char -> Diagram B
char c = text [c] # withEnvelope (rect 0.7 1 :: D V2 Double)
