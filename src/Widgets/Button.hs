module Widgets.Button where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import qualified Reactive
import Reactive (Reactive(..))
import qualified Event
import Utils (isInside)
import FormUtils

type Model = Bool

type Event = Bool

construct :: Model
construct = False

view :: Form -> Model -> Reactive (Model, Event)
view buttonIcon active =
    Reactive.onEvent
      (mouseAction (Event.buttonGuard MBLeft handleMouse))
      reactive
  where
    reactive = noEvent <$> Reactive.fromModel (render buttonIcon) active
    noEvent model = (model, False)
    mouseAction =
      if active
        then Event.mouseRelease
        else Event.mousePress
    handleMouse pos (active, clickEvent) = -- we are shadowing >.<
      if active
        -- we only send a "clicked" event for the button, if mouse was inside the button on release!
        then (False, isInside reactive pos)
        -- if we were not active, we activate if the mouse click was onto the button:
        else (isInside reactive pos, clickEvent) -- clickEvent can only be False at the moment

render :: Form -> Model -> Form
render buttonIcon active =
  (if active then addBackground lightGrey else id)
    (addBorder lightBlue buttonIcon)

isActive :: Model -> Bool
isActive = id
