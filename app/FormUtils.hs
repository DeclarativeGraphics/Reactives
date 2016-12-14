module FormUtils where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape

addBorder :: RGB -> Form -> Form
addBorder color form =
  outlined (solid color) rect `atop` form
  where
    rect = rectangleFromBB (Border.getBoundingBox (getBorder form))
