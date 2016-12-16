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
  outlined (solid color) (shapeFromForm form) `atop` form

addBackground :: RGB -> Form -> Form
addBackground color form =
  form `atop` filled color (shapeFromForm form)

shapeFromForm :: Form -> Bordered Shape
shapeFromForm form =
  rectangleFromBB (Border.getBoundingBox (getBorder form))

separator :: (a -> b -> c) -> (Double -> Double -> Form) -> Reactive a -> Reactive b -> Reactive c
separator combine separatorFromDists reactiveA reactiveB =
    Reactive.besidesTo down combine
      (Reactive.attachFormTo down (separatorFromDists maxLeft maxRight) reactiveA)
      reactiveB
  where
    bd dir reactive = Border.borderDistance (getBorder reactive) dir
    maxLeft = max (bd left reactiveA) (bd left reactiveB)
    maxRight = max (bd right reactiveA) (bd right reactiveB)

withParens :: TextStyle -> Reactive a -> Reactive a
withParens style reactive =
  Reactive.attachFormTo left (text style "(") $
  Reactive.attachFormTo right (text style ")")
  reactive
