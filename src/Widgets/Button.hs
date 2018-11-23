module Widgets.Button where

import Graphics.Declarative.Transforms
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
    reactive = Reactive.static (render buttonIcon active)
    mouseAction =
      if active
        then Event.mouseRelease
        else Event.mousePress
    handleMouse pos =
      case (active, isInside reactive pos) of
        -- when we are inactive and receive an event, it must be a release event
        (True, inside) -> Just (True, inside)
        -- when we receive a press event inside of our reactive, we activate
        (False, True) -> Just (True, False)
        -- in any other case, we don't do anything
        _ -> Nothing

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
