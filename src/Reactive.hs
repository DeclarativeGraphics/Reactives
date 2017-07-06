module Reactive where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import Data.Lens
import Utils (orElse, orTry, isInside, leftAngle, rightAngle)
import Control.Monad (join, liftM2)

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

emptyR :: a -> Reactive a
emptyR value = constant value empty

constant :: a -> Form -> Reactive a
constant value form = pure value <* irreactive form

fromModel :: (a -> Form) -> a -> Reactive a
fromModel produceForm model = pure model <* irreactive (produceForm model)

onEvent :: (Input -> b -> a) -> Reactive b -> Reactive a
onEvent eventHandler reactive =
    Reactive reaction (visual reactive)
  where
    reaction event = eventHandler event (react reactive event)

wrapFilterOutsideEvents :: (m -> Reactive m) -> m -> Reactive m
wrapFilterOutsideEvents view model =
    Reactive.onEvent handleEvent innerReactive
  where
    innerReactive = view model
    handleEvent event newModel
      | eventInside innerReactive event = newModel
      | otherwise = model

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

-- kinda like <*>, but with a flowing direction (V2 Double)
attach :: V2 Double -> Reactive (a -> b) -> Reactive a -> Reactive b
attach dir = besidesTo dir ($)

besidesAll :: V2 Double -> [Reactive a] -> Reactive [a]
besidesAll dir = atopAllReactives . placedBesidesTo dir

atopAllReactives :: [Reactive a] -> Reactive [a]
atopAllReactives = foldr (atopReactives (:)) (constant [] empty)

separatedBy :: V2 Double -> Form -> [Reactive a] -> Reactive [a]
separatedBy dir seperator [] = constant [] empty
separatedBy dir seperator (x:xs) =
    atopAllReactives (placedBesidesTo dir withSeperators)
  where
    withSeperators = x : map (attachFormTo (-dir) seperator) xs

onVisual :: (Form -> Form) -> Reactive a -> Reactive a
onVisual change reactive = reactive { visual = change (visual reactive) }

modifyEventPositions :: M33 Double -> Reactive a -> Reactive a
modifyEventPositions matrix reactive =
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
    onVisual (transformBy mat) $ modifyEventPositions (inv33 mat) reactive

instance HasBorder (Reactive a) where
  getBorder = getBorder . visual

instance Monoid a => Combinable (Reactive a) where
  empty = Reactive.constant mempty empty
  reactiveA `atop` reactiveB = atopReactives mappend reactiveA reactiveB
