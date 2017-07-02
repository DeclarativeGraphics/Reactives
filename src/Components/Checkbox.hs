module Components.Checkbox where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys

import Component
import qualified Event

make :: Bool -> Bool -> Component Bool
make focused checked = Component
    { dispatch = \event -> make focused (nextValue event)
    , render = text defaultTextStyle (if checked then "☑" else "☐")
    , value = checked
    , focusEvent = \newFocus -> make newFocus checked
    , focused = focused
    }
  where
    nextValue event =
      Event.mousePress
        (Event.buttonGuard MBLeft
          (\_ checked -> not checked))
        event
        checked
