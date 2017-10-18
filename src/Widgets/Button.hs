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

view :: Form -> Model -> Reactive Input (Model, Event)
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

viewIgnoreEvent :: Form -> Model -> Reactive Input Model
viewIgnoreEvent buttonIcon model = fst <$> view buttonIcon model

render :: Form -> Model -> Form
render buttonIcon active =
  (if active then addBackground lightGrey else id)
    (addBorder lightBlue buttonIcon)

isActive :: Model -> Bool
isActive = id

onClickEmit :: a -> Reactive Input (Model, Event) -> Reactive Input (Model, Maybe a)
onClickEmit something = fmap handleEvent
  where
    handleEvent (model, clicked) = (model, if clicked then Just something else Nothing)

handleButtonList :: (b -> Event) -> [b] -> ([b], Maybe b)
handleButtonList extractEvent ls =
  case ls of
    [] -> ([], Nothing)
    (e : rest) ->
      if extractEvent e -- button was clicked
        then (e : rest, Just e)
        else onFst ((:) e) (handleButtonList extractEvent rest)
  where
    onFst f (x, y) = (f x, y)
