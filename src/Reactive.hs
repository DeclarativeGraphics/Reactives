module Reactive where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)

import Data.Lens
import Control.Monad (join)

data Reactive msg
  = Reactive
  { react :: Input -> msg
  , visual :: Form
  }

type Pos = (Double, Double)

unit :: Form -> Reactive ()
unit form = Reactive
  { react = const ()
  , visual = form
  }

constant :: a -> Form -> Reactive a
constant value form = pure value <* unit form

onEvent :: (Input -> a) -> Reactive a -> Reactive a
onEvent reaction reactive =
  Reactive reaction (visual reactive)

click :: MB -> a -> Input -> Maybe a
click mouseButton value (MouseInput (MousePress _ button))
  | button == mouseButton = Just value
click _ _ _ = Nothing

eventInside :: Reactive a -> Reactive (Maybe a)
eventInside reactive =
    reactive { react = fmap (react reactive) . whenInside }
  where
    (topLeft, bottomRight) =
      Border.getBoundingBox $ getBorder $ visual reactive
    purelyPositive (x, y) = x >= 0 && y >= 0
    isInside pos =
      purelyPositive (topLeft `Vec2.to` pos) &&
      purelyPositive (pos `Vec2.to` bottomRight)
    whenInside (MouseInput m)
      | isInside (get mouseInputPos m) = Just $ MouseInput m
      | otherwise = Nothing
    whenInside e = Just e

joinFilters :: Reactive (Maybe (Maybe a)) -> Reactive (Maybe a)
joinFilters = fmap join

atopReactives :: Reactive a -> Reactive b -> Reactive (a, b)
atopReactives reactiveA reactiveB =
    Reactive reaction (atop (visual reactiveA) (visual reactiveB))
  where
    reaction event = (react reactiveA event, react reactiveB event)

onVisual :: (Form -> Form) -> Reactive a -> Reactive a
onVisual change reactive = reactive { visual = change (visual reactive) }

modifyEvents :: (Vec2 -> Vec2) -> Reactive a -> Reactive a
modifyEvents modifier reactive =
    reactive { react = react reactive . offsetEvent }
  where
    offsetEvent (MouseInput m) =
      MouseInput $ modify mouseInputPos modifier m
    offsetEvent e = e

moveR :: Vec2 -> Reactive a -> Reactive a
moveR offset reactive =
  onVisual (move offset) (modifyEvents (`Vec2.sub` offset) reactive)

instance Functor Reactive where
  fmap f (Reactive reaction visual) = Reactive (f . reaction) visual

instance Applicative Reactive where
  pure value = Reactive (const value) empty

  (Reactive reactFunc visual1) <*> (Reactive reactArg visual2)
    = Reactive react (atop visual1 visual2)
    where react event = reactFunc event (reactArg event)
