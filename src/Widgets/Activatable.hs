module Widgets.Activatable where

import Graphics.Declarative.Transforms
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import qualified Event
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
  let mousePressLeftGuard = Event.mouseInput . Event.mousePressGuard . Event.buttonGuard MBLeft
  in
    case activeOr of
      Active active ->
        let activeReactive = viewActive active
            outsideReactiveEvent = mousePressLeftGuard . Event.outsideGuard activeReactive . Just
        in 
          Reactive.onEvent
            (outsideReactiveEvent (Inactive (makeInactive active)))
            (Active <$> activeReactive)
      Inactive inactive ->
        let inactiveReactive = Inactive <$> viewInactive inactive
            insideReactiveEvent pos
              | isInside inactiveReactive pos = Just (Active (makeActive pos inactive))
              | otherwise = Nothing
        in
          Reactive.onEvent
            (mousePressLeftGuard insideReactiveEvent)
            inactiveReactive
