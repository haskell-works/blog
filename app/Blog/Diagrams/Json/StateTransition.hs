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

diaStateTransition :: Diagram B
diaStateTransition = vsep 50
  [ hsep 50
    [ circle 10 # named "A"
    , circle 10 # named "B"
    ]
  , hsep 50
    [ circle 10 # named "C"
    , circle 10 # named "D"
    ]
  ] # center
    # pad 1.6
    # connectPerim' ( with
        & arrowShaft .~ arc xDir ((-1/6) @@ turn)
        ) "A" "B" ((-15/16) @@ turn) (7/16 @@ turn)

genFiles :: IO ()
genFiles = svgToFile "images/gen/hw-json/state-transition.svg"           (mkWidth 1024) diaStateTransition
