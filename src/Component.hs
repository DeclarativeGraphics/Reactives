module Component where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys
import Linear

data Component a
  = Component
  { dispatch :: Input -> Component a
  , render :: Form
  , value :: a
  , focusEvent :: Bool -> Component a
  , focused :: Bool
  }

instance Functor Component where
  fmap = mapValue

mapValue :: (a -> b) -> Component a -> Component b
mapValue mapper component = Component
    { dispatch = mapValue mapper . dispatch component
    , render = render component
    , value = mapper (value component)
    , focusEvent = mapValue mapper . focusEvent component
    , focused = focused component
    }

attachFormTo :: V2 Double -> Form -> Component a -> Component a
attachFormTo dir attachment component = Component
    { dispatch = attachFormTo dir attachment . dispatch component
    , render = appendTo dir [render component, attachment]
    , value = value component
    , focusEvent = attachFormTo dir attachment . focusEvent component
    , focused = focused component
    }
