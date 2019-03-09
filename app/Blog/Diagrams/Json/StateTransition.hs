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
    [ circle 80
      # center
      # named "A"
      # showEnvelope' (with & ePoints .~ 360)
      # showOrigin
      # orbit (unitX # rotateBy (2/6) # scale 2.2) (text "Hello worlds" # fontSize (normalized 0.05))
    , circle 80 # named "B"
    ]
  , hsep 200
    [ circle 80 # named "C"
    , circle 80 # named "D"
    ]
  ] # center
    # pad 1.6
    # connectPerim' (with & arrowShaft .~ arc xDir ((-1/6) @@ turn) & arrowHead .~ dart & headLength .~ small) "A" "B" ((-15/16) @@ turn) (7/16 @@ turn)
    # connectPerim' (with & arrowShaft .~ arc xDir ((-4/6) @@ turn) & arrowHead .~ dart & headLength .~ small) "A" "A" ((  6/16) @@ turn) (4/16 @@ turn)

genFiles :: IO ()
genFiles = svgToFile "images/gen/hw-json/state-transition.svg"           (mkWidth 1024) diaStateTransition
