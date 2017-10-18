module Widgets.Activatable where

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
import Linear

data ActiveOr active inactive
  = Active active
  | Inactive inactive
  deriving (Show, Eq, Read)

view
  :: (act -> inact)
  -> (V2 Double -> inact -> act)
  -> (act -> Reactive Input act)
  -> (inact -> Reactive Input inact)
  -> ActiveOr act inact
  -> Reactive Input (ActiveOr act inact)
view makeInactive makeActive viewActive viewInactive activeOr =
  case activeOr of
    (Active active) ->
      let activeReactive = viewActive active
          maybeMakeInactive (MouseInput (MousePress pos MBLeft))
            | not (isInside activeReactive pos) =
              Inactive . makeInactive
          maybeMakeInactive event = Active
       in Reactive.onEvent maybeMakeInactive activeReactive
    (Inactive inActive) ->
      let inActiveReactive = viewInactive inActive
          maybeMakeActive (MouseInput (MousePress pos MBLeft))
            | isInside inActiveReactive pos = Active . makeActive pos
          maybeMakeActive event                  = Inactive
       in Reactive.onEvent maybeMakeActive inActiveReactive
