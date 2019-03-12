{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module Blog.Diagrams.Json.StateRailroad where

import Blog.Data.Json.StateMachine
import Blog.Doc.Frame
import Blog.Doc.Json.Text
import Blog.Svg
import Diagrams.Backend.SVG
import Diagrams.Prelude

import qualified System.Directory as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Reundant do"        :: String) #-}

transitionFor :: Char -> (State, State, State, State)
transitionFor c =
  ( stateMachine c InJson
  , stateMachine c InValue
  , stateMachine c InString
  , stateMachine c InEscape
  )

transitionDiagram :: Char -> (State, State, State, State) -> Diagram B
transitionDiagram c (InJson  , InJson  , InString, InString) = transition1 [c]
transitionDiagram c (InString, InString, InJson  , InString) = transition2 [c]
transitionDiagram c (InValue , InValue , InString, InString) = transition3 [c]
transitionDiagram c (InJson  , InJson  , InEscape, InString) = transition4 [c]
transitionDiagram c _                                        = transition0 [c]

selectTransition :: Char -> Diagram B
selectTransition c = transitionDiagram c (transitionFor c)

fullRailroadDiagram :: Diagram B
fullRailroadDiagram = hsep 0 (fmap selectTransition jsonText) # center

eachTransition :: Diagram B
eachTransition = vsep 1
  [ hsep 1
    [ legend
    , transition1 "A"
    , transition2 "B"
    , transition3 "C"
    , transition4 "D"
    ] # scale 2 # center
  ] # withName "in-escape" (\s d -> d # fc blue)
  where legend = mconcat
          [ text "Escape" # translateY 6 # alignR
          , text "String" # translateY 4 # alignR
          , text "Value"  # translateY 2 # alignR
          , text "Json"   # translateY 0 # alignR
          , rect 3 9      # translateY 0 # opacity 0
          ]

smSingleTransition :: Int -> Int -> QDiagram B V2 Double Any
smSingleTransition a b = fromSegments
  [ bézier3
      (r2 (1, 0))
      (r2 (2, fromIntegral (bx - ax)))
      (r2 (3, fromIntegral (bx - ax)))
  ] # translateY (min (fromIntegral bx) (fromIntegral ax) + max (fromIntegral (ax - bx)) 0)
  where ax = 2 * a
        bx = 2 * b

transition0 :: String -> QDiagram B V2 Double Any
transition0 label = mconcat
  [ vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , [ smSingleTransition 3 3 # lc black
    , smSingleTransition 2 2 # lc black
    , smSingleTransition 1 1 # lc black
    , smSingleTransition 0 0 # lc black
    ] # mconcat # opacityGroup 0.5
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc black
  ]

transition1 :: String -> QDiagram B V2 Double Any
transition1 label = mconcat
  [ vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , [ smSingleTransition 3 2 # lc green
    , smSingleTransition 2 2 # lc green
    , smSingleTransition 1 0 # lc green
    , smSingleTransition 0 0 # lc green
    ] # mconcat # opacityGroup 0.5
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc green
  ]

transition2 :: String -> QDiagram B V2 Double Any
transition2 label = mconcat
  [ vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , [ smSingleTransition 3 2 # lc orange
    , smSingleTransition 2 0 # lc orange
    , smSingleTransition 1 2 # lc orange
    , smSingleTransition 0 2 # lc orange
    ] # mconcat # opacityGroup 0.5
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc orange
  ]

transition3 :: String -> QDiagram B V2 Double Any
transition3 label = mconcat
  [ vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , [ smSingleTransition 3 2 # lc brown
    , smSingleTransition 2 2 # lc brown
    , smSingleTransition 1 1 # lc brown
    , smSingleTransition 0 1 # lc brown
    ] # mconcat # opacityGroup 0.5
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc brown
  ]

transition4 :: String -> QDiagram B V2 Double Any
transition4 label = mconcat
  [ vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , [ smSingleTransition 3 2 # lc blue
    , smSingleTransition 2 3 # lc blue
    , smSingleTransition 1 0 # lc blue
    , smSingleTransition 0 0 # lc blue
    ] # mconcat # opacityGroup 0.5
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc blue
  ]

jsonText :: String
jsonText = "{\"key\": [12, \"[\\\"a\\\"]\"]}"

diaRank1 :: Diagram B
diaRank1 = rect 16 12 # lc white <> body # font "Consolas,monaco,monospace"
  where body :: Diagram B
        body = highlightText "abc" "" []

genFiles :: IO ()
genFiles = do
  svgToFile "images/gen/hw-json/rank-1.svg"           (mkWidth 1024) diaRank1
  svgToFile "images/gen/hw-json/full-railroad.svg"    (mkWidth 1024) fullRailroadDiagram
  svgToFile "images/gen/hw-json/each-transition.svg"  (mkWidth 1024) eachTransition
