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
    [ (text "J" <> circle 80 # fc cyan) # named "J"
    , (text "V" <> circle 80 # fc cyan) # named "V"
    ]
  , hsep 200
    [ (text "S" <> circle 80 # fc cyan) # named "S"
    , (text "E" <> circle 80 # fc cyan) # named "E"
    ]
  ] # center
    # pad 1.4
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "E" "S" ( 9/16 @@ turn) (15/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir (-10/16 @@ turn)) "J" "J" ( 6/16 @@ turn) ( 4/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "J" "S" (13/16 @@ turn) ( 3/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "J" "V" ( 1/16 @@ turn) ( 7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "S" "E" ( 1/16 @@ turn) ( 7/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "S" "J" ( 5/16 @@ turn) (11/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir (-10/16 @@ turn)) "S" "S" (11/16 @@ turn) ( 9/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir ( -2/16 @@ turn)) "V" "J" ( 9/16 @@ turn) (15/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small                                          ) "V" "S" (10/16 @@ turn) ( 2/16 @@ turn)
    # connectPerim' (with & arrowHead .~ dart & headLength .~ small & arrowShaft .~ arc xDir (-10/16 @@ turn)) "V" "V" ( 3/16 @@ turn) ( 1/16 @@ turn)
    # withName "J" (\s -> atop (text "JX" # moveTo (location s) # moveTo ( (-70) ^&   140 )))
    # withName "J" (\s -> atop (text "JY" # moveTo (location s) # moveTo (  180  ^&    80 )))
    # withName "J" (\s -> atop (text "JZ" # moveTo (location s) # moveTo (   80  ^& (-180))))
    # withName "V" (\s -> atop (text "VX" # moveTo (location s) # moveTo (  140  ^&    70 )))
    # withName "V" (\s -> atop (text "VY" # moveTo (location s) # moveTo ((-180) ^&  (-80))))
    # withName "S" (\s -> atop (text "SX" # moveTo (location s) # moveTo (  180  ^&    80 )))
    # withName "S" (\s -> atop (text "SY" # moveTo (location s) # moveTo ( (-80) ^&   180 )))
    # withName "S" (\s -> atop (text "SZ" # moveTo (location s) # moveTo ( (-70) ^& (-140))))
    # withName "E" (\s -> atop (text "EY" # moveTo (location s) # moveTo ((-180) ^&  (-80))))
    # fontSize (normalized 0.05)

genFiles :: IO ()
genFiles = svgToFile "images/gen/hw-json/state-transition.svg"           (mkWidth 1024) diaStateTransition
