module FormUtils where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape

import qualified Reactive
import Reactive (Reactive(..))

addBorder :: RGB -> Form -> Form
addBorder color form =
  outlined (solid color) rect `atop` form
  where
    rect = rectangleFromBB (Border.getBoundingBox (getBorder form))

separator :: (a -> b -> c) -> (Double -> Double -> Form) -> Reactive a -> Reactive b -> Reactive c
separator combine separatorFromDists reactiveA reactiveB =
    Reactive.besidesTo down combine
      (Reactive.attachFormTo down (separatorFromDists maxLeft maxRight) reactiveA)
      reactiveB
  where
    bd dir reactive = Border.borderDistance (getBorder reactive) dir
    maxLeft = max (bd left reactiveA) (bd left reactiveB)
    maxRight = max (bd right reactiveA) (bd right reactiveB)
