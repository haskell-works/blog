{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module Blog.Diagrams.Json.StateTransition where

import Blog.Data.Json.StateMachine
import Blog.Doc.Frame
import Blog.Doc.Json.Text
import Blog.Svg
import Data.Maybe
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

connectPerim''
  :: (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
  => ArrowOpts n -> n1 -> n2 -> Angle n -> Angle n
  -> (QDiagram b V2 n Any -> QDiagram b V2 n Any)
  -> QDiagram b V2 n Any -> QDiagram b V2 n Any
connectPerim'' opts n1 n2 a1 a2 f =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [os, oe] = map location [sub1, sub2]
        s = fromMaybe os (maxTraceP os (unitX # rotate a1) sub1)
        e = fromMaybe oe (maxTraceP oe (unitX # rotate a2) sub2)
    in  atop (f (arrowBetween' opts s e))

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
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "E" "S" ( 9/16 @@ turn) (15/16 @@ turn) (\x -> x # showEnvelope # orbit (unitX # rotateBy 0) (square 20) # lc red  )
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir (-10/16 @@ turn)) "J" "J" ( 6/16 @@ turn) ( 4/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc green)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "J" "S" (13/16 @@ turn) ( 3/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc blue)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "J" "V" ( 1/16 @@ turn) ( 7/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc purple)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "S" "E" ( 1/16 @@ turn) ( 7/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc orange)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "S" "J" ( 5/16 @@ turn) (11/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc yellow)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir (-10/16 @@ turn)) "S" "S" (11/16 @@ turn) ( 9/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc cyan)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "V" "J" ( 9/16 @@ turn) (15/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc gold)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small                                          ) "V" "S" (10/16 @@ turn) ( 2/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc pink)
    # connectPerim'' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir (-10/16 @@ turn)) "V" "V" ( 3/16 @@ turn) ( 1/16 @@ turn) (\x -> x #                orbit (unitX # rotateBy (2/6)) (square 20) # lc aqua)
    # fontSize (normalized 0.05)

genFiles :: IO ()
genFiles = svgToFile "images/gen/hw-json/state-transition.svg"           (mkWidth 1024) diaStateTransition
