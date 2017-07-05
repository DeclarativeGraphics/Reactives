module Components.List where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys
import Linear
import Utils (orElse)
import FormUtils

import Component
import qualified Event

import qualified Components.Beside as Beside

make :: V2 Double -> Component a -> [Component a] -> Component [a]
make dir component [] = (\x -> [x]) <$> component
make dir component (other : rest) =
  uncurry (:) <$> Beside.makeHidden dir component (make dir other rest)
