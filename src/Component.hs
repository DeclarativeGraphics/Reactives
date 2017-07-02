module Component where

import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.SDL.Input
import Data.Function (fix)

data Component a
  = Component
  { dispatch :: Input -> Component a
  , render :: Form
  , value :: a
  , focusEvent :: Bool -> Component a
  , focused :: Bool
  }

mapValue :: (a -> b) -> Component a -> Component b
mapValue mapper component = Component
    { dispatch = mapValue mapper . dispatch component
    , render = render component
    , value = mapper (value component)
    , focusEvent = mapValue mapper . focusEvent component
    , focused = focused component
    }
