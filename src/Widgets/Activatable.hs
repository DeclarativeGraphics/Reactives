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
  | InActive inactive

view
  :: (act -> inact)
  -> (inact -> act)
  -> (act -> Reactive act)
  -> (inact -> Reactive inact)
  -> ActiveOr act inact
  -> Reactive (ActiveOr act inact)
view makeInActive makeActive viewActive viewInActive activeOr =
  case activeOr of
    (Active active) ->
      let activeReactive = viewActive active
          maybeMakeInactive (MouseInput (MousePress pos MBLeft))
            | not (isInside activeReactive pos) =
              InActive . makeInActive
          maybeMakeInactive event = Active
       in Reactive.onEvent maybeMakeInactive activeReactive
    (InActive inActive) ->
      let inActiveReactive = viewInActive inActive
          maybeMakeActive (MouseInput (MousePress pos MBLeft))
            | isInside inActiveReactive pos = Active . makeActive
          maybeMakeActive event                  = InActive
       in Reactive.onEvent maybeMakeActive inActiveReactive
