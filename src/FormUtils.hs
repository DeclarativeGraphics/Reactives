module FormUtils where

import Graphics.Declarative.Transforms
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape

import qualified Reactive
import Reactive (Reactive(..))

addBorder :: RGB -> Form -> Form
addBorder color form =
  outlined (solid color) (shapeFromForm form) <> form

addBackground :: RGB -> Form -> Form
addBackground color form =
  form <> filled color (shapeFromForm form)

shapeFromForm :: Form -> Bordered Shape
shapeFromForm form =
  rectangleFromBB (Border.getBoundingBox (getBorder form))

separator :: Transformable e => (Double -> Double -> Form) -> Reactive e a -> Reactive e a -> Reactive e a
separator separatorFromDists reactiveA reactiveB =
    appendTo down
      [ reactiveA
      , Reactive.static (separatorFromDists maxLeft maxRight)
      , reactiveB
      ]
  where
    bd dir reactive = Border.borderDistance (getBorder reactive) dir
    maxLeft = max (bd left reactiveA) (bd left reactiveB)
    maxRight = max (bd right reactiveA) (bd right reactiveB)

withParens :: Transformable e => TextStyle -> Reactive e a -> Reactive e a
withParens style reactive =
  appendTo right
    [ Reactive.static (text style "(")
    , reactive
    , Reactive.static (text style ")")
    ]
