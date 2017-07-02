module Components.Beside where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys
import Linear
import Utils (orElse, isInside)
import Data.Lens

import Component
import qualified Event

makeHidden :: V2 Double -> Component a -> Component b -> Component (a, b)
makeHidden dir componentLeft componentRight =
  mapValue toTouple (make dir False (Beside componentLeft componentRight FocusNeither))

make :: V2 Double -> Bool -> Beside (Component a) (Component b) -> Component (Beside a b)
make dir focused beside = Component
    { dispatch = make dir focused . nextValue
    , render = appendTo dir [render (leftBeside beside), render (rightBeside beside)]
    , value = mapBeside value value beside
    , focusEvent = \newFocus -> make dir newFocus beside
    , focused = focused
    }
  where
    nextValue event =
      delegateEvent event
        (Event.mousePress
          (Event.buttonGuard MBLeft handleFocusChange)
          event
          beside)
    renderLeftAlone = render (leftBeside beside)
    renderRightAlone = moveBesideBy (displacementTo dir) renderLeftAlone (render (rightBeside beside))
    handleFocusChange pos besides
      | isInside renderLeftAlone pos = focusLeft besides
      | isInside renderRightAlone pos = focusRight besides
      | otherwise = focusNeither besides
    -- TODO: Mouse inputs should ignore who has the "focus". Thats just for keyboard events.
    delegateEvent event (Beside componentA componentB FocusLeft) =
      Beside (dispatch componentA event) componentB FocusLeft
    delegateEvent event (Beside componentA componentB FocusRight) =
      Beside componentA (dispatch componentB (offsetEvent (moveMatrix (-(displacement componentA componentB))) event)) FocusRight
    delegateEvent event besides = besides

    displacement compLeft compRight =
      displacementTo dir (getBorder (render compLeft)) (getBorder (render compRight))

data Beside a b
  = Beside
  { leftBeside :: a
  , rightBeside :: b
  , focus :: Focus
  }

data Focus = FocusLeft | FocusRight | FocusNeither

toTouple :: Beside a b -> (a, b)
toTouple (Beside a b _) = (a, b)

mapBeside :: (a -> u) -> (b -> v) -> Beside a b -> Beside u v
mapBeside f g (Beside a b foc) = Beside (f a) (g b) foc

focusLeft :: Beside (Component a) (Component b) -> Beside (Component a) (Component b)
focusLeft (Beside a b FocusLeft) = Beside a b FocusLeft
focusLeft (Beside a b _) = Beside (focusEvent a True) (focusEvent b False) FocusLeft

focusRight :: Beside (Component a) (Component b) -> Beside (Component a) (Component b)
focusRight (Beside a b FocusRight) = Beside a b FocusRight
focusRight (Beside a b _) = Beside (focusEvent a False) (focusEvent b True) FocusRight

focusNeither :: Beside (Component a) (Component b) -> Beside (Component a) (Component b)
focusNeither (Beside a b FocusNeither) = Beside a b FocusNeither
focusNeither (Beside a b _) = Beside (focusEvent a False) (focusEvent b False) FocusNeither

offsetEvent :: M33 Double -> Input -> Input
offsetEvent matrix (MouseInput m) = MouseInput $ modify mouseInputPos transformPos m
  where transformPos (V2 x y) = toV2 $ matrix !* (V3 x y 1)
offsetEvent matrix e = e
