{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module Blog.Diagrams.Json.StateTransition where

import Blog.Data.Json.StateMachine
import Blog.Doc.Frame
import Blog.Doc.Json.Text
import Blog.Svg
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Arrowheads

import qualified System.Directory as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Reundant do"        :: String) #-}

orbit :: ()
  => Metric         (V a)
  => Additive       (V a)
  => Eq             (N a)
  => Fractional     (N a)
  => Floating       (N a)
  => Transformable  a
  => Juxtaposable   a
  => Semigroup      a
  => Vn a -> a -> a -> a
orbit v b a = a <> juxtapose v (a # scale (norm v)) b

diaStateTransition :: Diagram B
diaStateTransition = vsep 200
  [ hsep 200
    [ (circle 80 <> text "J")
      # center
      # named "J"
      # orbit (unitX # rotateBy (2/6) # scale 2.2) (text "Hello worlds" # fontSize (normalized 0.05))
    , (circle 80 <> text "V") # named "V"
    ]
  , hsep 200
    [ (circle 80 <> text "S") # named "S"
    , (circle 80 <> text "E") # named "E"
    ]
  ] # center
    # pad 1.6
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ((-1/6) @@ turn)) "J" "V" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ((-4/6) @@ turn)) "J" "J" ((  6/16) @@ turn) (4/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "E" "S" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "J" "J" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "J" "S" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "J" "V" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "S" "E" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "S" "J" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "S" "S" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "V" "J" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "V" "S" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "V" "V" ((-15/16) @@ turn) (7/16 @@ turn)
    # fontSize (normalized 0.05)

genFiles :: IO ()
genFiles = svgToFile "images/gen/hw-json/state-transition.svg"           (mkWidth 1024) diaStateTransition
