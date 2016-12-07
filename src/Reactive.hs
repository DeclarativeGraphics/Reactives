module Reactive where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import Data.Lens
import Utils (orElse, orTry)
import Control.Monad (join)

import Linear

data Reactive msg
  = Reactive
  { react :: Input -> msg
  , visual :: Form
  }

type Pos = (Double, Double)

irreactive :: Form -> Reactive ()
irreactive form = Reactive
  { react = const ()
  , visual = form
  }

constant :: a -> Form -> Reactive a
constant value form = pure value <* irreactive form

onEvent :: (Input -> a) -> Reactive b -> Reactive a
onEvent reaction reactive =
  Reactive reaction (visual reactive)

andOnEvent :: (Input -> Maybe a) -> Reactive (Maybe a) -> Reactive (Maybe a)
andOnEvent eventHandler reactive =
    Reactive reaction (visual reactive)
  where
    reaction event =
      (eventHandler event) `orTry`
      (react reactive event)

click :: MB -> a -> Input -> Maybe a
click mouseButton value (MouseInput (MousePress _ button))
  | button == mouseButton = Just value
click _ _ _ = Nothing

mouseMove :: (V2 Double -> a) -> Input -> Maybe a
mouseMove callback (MouseInput (MouseMove pos)) = Just (callback pos)
mouseMove _ _ = Nothing

andFilterOutside :: Reactive (Maybe a) -> Reactive (Maybe a)
andFilterOutside = fmap join . filterOutside

filterOutside :: Reactive a -> Reactive (Maybe a)
filterOutside reactive =
    reactive { react = fmap (react reactive) . whenInside }
  where
    whenInside event
      | eventInside reactive event = Just event
      | otherwise = Nothing

isInside :: HasBorder a => a -> V2 Double -> Bool
isInside sth pos =
  purelyPositive (pos - topLeft) &&
  purelyPositive (bottomRight - pos)
  where
    (topLeft, bottomRight) =
      Border.getBoundingBox $ getBorder sth
    purelyPositive (V2 x y) = x >= 0 && y >= 0

eventInside :: HasBorder a => a -> Input -> Bool
eventInside sth (MouseInput m) = isInside sth (get mouseInputPos m)
eventInside sth _ = True

atopReactives :: (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
atopReactives combine reactiveA reactiveB =
    Reactive reaction (atop (visual reactiveA) (visual reactiveB))
  where
    reaction event = combine (react reactiveA event) (react reactiveB event)

-- Place some Form beside a reactive
attachFormTo :: V2 Double -> Form -> Reactive a -> Reactive a
attachFormTo dir attachment reactive = reactive <* movedIrreactive
  where
    movedIrreactive =
      moveBesideBy (displacementTo dir) reactive $
        Reactive.irreactive attachment

besidesTo :: V2 Double -> (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
besidesTo dir combine reference toBeMoved =
  Reactive.atopReactives combine reference moved
  where moved = moveBesideBy (displacementTo dir) reference toBeMoved

atopAllReactives :: [Reactive a] -> Reactive [a]
atopAllReactives = foldr (atopReactives (:)) (constant [] empty)

seperatedBy :: V2 Double -> Form -> [Reactive a] -> Reactive [a]
seperatedBy dir seperator [] = constant [] empty
seperatedBy dir seperator (x:xs) =
    atopAllReactives (placedBesidesTo dir withSeperators)
  where
    withSeperators = x : map (attachFormTo (-dir) seperator) xs

onVisual :: (Form -> Form) -> Reactive a -> Reactive a
onVisual change reactive = reactive { visual = change (visual reactive) }

modifyEvents :: M33 Double -> Reactive a -> Reactive a
modifyEvents matrix reactive =
    reactive { react = react reactive . offsetEvent }
  where
    transformPos (V2 x y) = toV2 $ matrix !* (V3 x y 1)
    offsetEvent (MouseInput m) =
      MouseInput $ modify mouseInputPos transformPos m
    offsetEvent e = e

instance Functor Reactive where
  fmap f (Reactive reaction visual) = Reactive (f . reaction) visual

instance Applicative Reactive where
  pure value = Reactive (const value) empty

  (Reactive reactFunc visual1) <*> (Reactive reactArg visual2)
    = Reactive react (atop visual1 visual2)
    where react event = reactFunc event (reactArg event)

instance Transformable (Reactive a) where
  transformBy mat reactive =
    onVisual (transformBy mat) $ modifyEvents (inv33 mat) reactive

instance HasBorder (Reactive a) where
  getBorder = getBorder . visual
