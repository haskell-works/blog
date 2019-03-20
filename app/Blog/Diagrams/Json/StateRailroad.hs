{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module Blog.Diagrams.Json.StateRailroad where

import Blog.Data.Json.StateMachine
import Blog.Doc.Frame
import Blog.Doc.Json.Text
import Blog.Svg
import Control.Applicative
import Data.Maybe
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
transitionDiagram c (InJson  , InJson  , InString, InString) = transition1 (repeat (lc green  . fc green )) [c]
transitionDiagram c (InString, InString, InJson  , InString) = transition2 (repeat (lc orange . fc orange)) [c]
transitionDiagram c (InValue , InValue , InString, InString) = transition3 (repeat (lc brown  . fc brown )) [c]
transitionDiagram c (InJson  , InJson  , InEscape, InString) = transition4 (repeat (lc blue   . fc blue  )) [c]
transitionDiagram c _                                        = transition0 (repeat (lc black  . fc black )) [c]

transitionDiagram' :: Char -> (State, State, State, State) -> [QDiagram B V2 Double Any -> QDiagram B V2 Double Any] -> Diagram B
transitionDiagram' c (InJson  , InJson  , InString, InString) cs = transition1 (fmap (lc green  . fc green ) cs) [c]
transitionDiagram' c (InString, InString, InJson  , InString) cs = transition2 (fmap (lc orange . fc orange) cs) [c]
transitionDiagram' c (InValue , InValue , InString, InString) cs = transition3 (fmap (lc brown  . fc brown ) cs) [c]
transitionDiagram' c (InJson  , InJson  , InEscape, InString) cs = transition4 (fmap (lc blue   . fc blue  ) cs) [c]
transitionDiagram' c _                                        cs = transition0 (fmap (lc black  . fc black ) cs) [c]

selectTransition :: Char -> Diagram B
selectTransition c = transitionDiagram c (transitionFor c)

selectTransition' :: Char -> [QDiagram B V2 Double Any -> QDiagram B V2 Double Any] -> Diagram B
selectTransition' c = transitionDiagram' c (transitionFor c)

fullRailroadDiagram :: Diagram B
fullRailroadDiagram = hsep 0 (fmap selectTransition jsonText) # center

fullRailroadDiagram2 :: Diagram B
fullRailroadDiagram2 = hsep 0 mkDiagram # center
  where mkDiagram = zipWith ($) (fmap selectTransition' jsonText) cs
        cs  = [ [ hi, id, id, id, id ]
              , [ hi, id, id, id, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, hi, id, id ]
              , [ hi, id, id, id, id ]
              , [ hi, id, id, id, id ]
              , [ hi, id, id, id, id ]
              , [ hi, id, id, id, id ]
              , [ id, hi, id, id, id ]
              , [ id, hi, id, id, id ]
              , [ hi, id, id, id, id ]
              , [ hi, id, id, id, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, id, hi, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, id, hi, id ]
              , [ id, id, hi, id, id ]
              , [ id, id, hi, id, id ]
              , [ hi, id, id, id, id ]
              , [ hi, id, id, id, id ]
              ] <> repeat [id, id, id, id, id]
        hi = lc black . lw 3

fullRailroadDiagram3a :: Diagram B
fullRailroadDiagram3a = hsep 0 mkDiagram # center
  where mkDiagram = zipWith ($) (fmap selectTransition' "\"[\\") cs
        cs  = [ [ id, id, id, id, id ]
              , [ id, id, id, id, id ]
              , [ id, id, id, id, id ]
              ] <> repeat [id, id, id, id, id]

fullRailroadDiagram3b :: Diagram B
fullRailroadDiagram3b = hsep 0 mkDiagram # center
  where mkDiagram = zipWith ($) (fmap selectTransition' "\"[\\") cs
        cs  = [ [ hi, hi, hi, hi, id ]
              , [ hi, id, hi, id, id ]
              , [ hi, id, hi, id, id ]
              ] <> repeat [id, id, id, id, id]
        hi = lc black . lw 8

eachTransition :: Diagram B
eachTransition = vsep 1
  [ hsep 1
    [ legend
    , transition1 (repeat (lc green )) "A"
    , transition2 (repeat (lc orange)) "B"
    , transition3 (repeat (lc brown )) "C"
    , transition4 (repeat (lc blue  )) "D"
    ] # scale 2 # center
  ] # withName "in-escape" (\s d -> d # fc blue)
  where legend = mconcat
          [ text "Escape" # translateY 6 # alignR
          , text "String" # translateY 4 # alignR
          , text "Value"  # translateY 2 # alignR
          , text "Json"   # translateY 0 # alignR
          , rect 3 9      # translateY 0 # opacity 0
          ]

eachTransition2 :: Diagram B
eachTransition2 = vsep 1
  [ hsep 1
    [ transition1 (repeat (lc green )) ""
    , transition2 (repeat (lc orange)) ""
    , transition3 (repeat (lc brown )) ""
    , transition4 (repeat (lc blue  )) ""
    ] # scale 2 # center
  ] # withName "in-escape" (\s d -> d # fc blue)

smSingleTransition :: Int -> Int -> QDiagram B V2 Double Any
smSingleTransition a b = fromSegments
  [ bézier3
      (r2 (1, 0))
      (r2 (2, fromIntegral (bx - ax)))
      (r2 (3, fromIntegral (bx - ax)))
  ] # translateY (min (fromIntegral bx) (fromIntegral ax) + max (fromIntegral (ax - bx)) 0)
  where ax = 2 * a
        bx = 2 * b

v = 3
e = 2
s = 1
j = 0

mkTransitionLabel :: String -> [QDiagram B V2 Double Any]
mkTransitionLabel "" = []
mkTransitionLabel label =
  [ text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc black
  ]

transition0 :: [QDiagram B V2 Double Any -> QDiagram B V2 Double Any] -> String -> QDiagram B V2 Double Any
transition0 cs label = mconcat $
  [ vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2)
  , vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2) # translateX 3
  , [ smSingleTransition v v # (cs !! 1)
    , smSingleTransition e e # (cs !! 3)
    , smSingleTransition s s # (cs !! 2)
    , smSingleTransition j j # (cs !! 0)
    ] # mconcat # opacityGroup 0.5
  ] <> mkTransitionLabel label
  where labelHeight = if label == "" then 0 else 2

transition1 :: [QDiagram B V2 Double Any -> QDiagram B V2 Double Any] -> String -> QDiagram B V2 Double Any
transition1 cs label = mconcat $
  [ vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2)
  , vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2) # translateX 3
  , [ smSingleTransition v j # (cs !! v)
    , smSingleTransition e s # (cs !! e)
    , smSingleTransition s s # (cs !! s)
    , smSingleTransition j j # (cs !! j)
    ] # mconcat # opacityGroup 0.5
  ] <> mkTransitionLabel label
  where labelHeight = if label == "" then 0 else 2

transition2 :: [QDiagram B V2 Double Any -> QDiagram B V2 Double Any] -> String -> QDiagram B V2 Double Any
transition2 cs label = mconcat $
  [ vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2)
  , vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2) # translateX 3
  , [ smSingleTransition v s # (cs !! v)
    , smSingleTransition e s # (cs !! e)
    , smSingleTransition s j # (cs !! s)
    , smSingleTransition j s # (cs !! j)
    ] # mconcat # opacityGroup 0.5
  ] <> mkTransitionLabel label
  where labelHeight = if label == "" then 0 else 2

transition3 :: [QDiagram B V2 Double Any -> QDiagram B V2 Double Any] -> String -> QDiagram B V2 Double Any
transition3 cs label = mconcat $
  [ vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2)
  , vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2) # translateX 3
  , [ smSingleTransition v v # (cs !! v)
    , smSingleTransition e s # (cs !! e)
    , smSingleTransition s s # (cs !! s)
    , smSingleTransition j v # (cs !! j)
    ] # mconcat # opacityGroup 0.5
  ] <> mkTransitionLabel label
  where labelHeight = if label == "" then 0 else 2

transition4 :: [QDiagram B V2 Double Any -> QDiagram B V2 Double Any] -> String -> QDiagram B V2 Double Any
transition4 cs label = mconcat $
  [ vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2)
  , vrule (labelHeight + 7) # lc silver # translateY (3 - labelHeight / 2) # translateX 3
  , [ smSingleTransition v j # (cs !! v)
    , smSingleTransition e s # (cs !! e)
    , smSingleTransition s e # (cs !! s)
    , smSingleTransition j j # (cs !! j)
    ] # mconcat # opacityGroup 0.5
  ] <> mkTransitionLabel label
  where labelHeight = if label == "" then 0 else 2

jsonText :: String
jsonText = "{\"key\": [12, \"[\\\"a\\\"]\"]}"

diaRank1 :: Diagram B
diaRank1 = rect 16 12 # lc white <> body # font "Consolas,monaco,monospace"
  where body :: Diagram B
        body = highlightText "abc" "" []

genFiles :: IO ()
genFiles = do
  svgToFile "images/gen/hw-json/rank-1.svg"             (mkWidth 1024) diaRank1
  svgToFile "images/gen/hw-json/full-railroad.svg"      (mkWidth 1024) fullRailroadDiagram
  svgToFile "images/gen/hw-json/full-railroad-2.svg"    (mkWidth 1024) fullRailroadDiagram2
  svgToFile "images/gen/hw-json/full-railroad-3-a.svg"  (mkWidth 1024) fullRailroadDiagram3a
  svgToFile "images/gen/hw-json/full-railroad-3-b.svg"  (mkWidth 1024) fullRailroadDiagram3b
  svgToFile "images/gen/hw-json/each-transition.svg"    (mkWidth 1024) eachTransition
  svgToFile "images/gen/hw-json/each-transition-2.svg"  (mkWidth 1024) eachTransition2
