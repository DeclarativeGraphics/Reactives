module Reactive where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import Control.Lens
import Utils (orElse, orTry, isInside, leftAngle, rightAngle)
import Control.Monad (join, liftM2)

import Linear

data Reactive event msg
  = Reactive
  { react :: event -> msg
  , visual :: Form
  }

type Pos = (Double, Double)

irreactive :: Form -> Reactive e ()
irreactive form = Reactive
  { react = const ()
  , visual = form
  }

emptyR :: a -> Reactive e a
emptyR value = constant value empty

constant :: a -> Form -> Reactive e a
constant value form = pure value <* irreactive form

fromModel :: (a -> Form) -> a -> Reactive e a
fromModel produceForm model = pure model <* irreactive (produceForm model)

onEvent :: (e -> b -> a) -> Reactive e b -> Reactive e a
onEvent eventHandler reactive =
    Reactive reaction (visual reactive)
  where
    reaction event = eventHandler event (react reactive event)

wrapFilterOutsideEvents :: (m -> Reactive Input m) -> m -> Reactive Input m
wrapFilterOutsideEvents view model =
    Reactive.onEvent handleEvent innerReactive
  where
    innerReactive = view model
    handleEvent event newModel
      | eventInside innerReactive event = newModel
      | otherwise = model

eventInside :: HasBorder a => a -> Input -> Bool
eventInside sth (MouseInput m) = isInside sth (view mouseInputPos m)
eventInside sth _ = True

atopReactives :: (a -> b -> c) -> Reactive e a -> Reactive e b -> Reactive e c
atopReactives combine reactiveA reactiveB =
    Reactive reaction (atop (visual reactiveA) (visual reactiveB))
  where
    reaction event = combine (react reactiveA event) (react reactiveB event)

-- Place some Form beside a reactive
attachFormTo :: Transformable e => V2 Double -> Form -> Reactive e a -> Reactive e a
attachFormTo dir attachment reactive = reactive <* movedIrreactive
  where
    movedIrreactive =
      moveBesideBy (displacementTo dir) reactive $
        Reactive.irreactive attachment

besidesTo :: Transformable e => V2 Double -> (a -> b -> c) -> Reactive e a -> Reactive e b -> Reactive e c
besidesTo dir combine reference toBeMoved =
  Reactive.atopReactives combine reference moved
  where moved = moveBesideBy (displacementTo dir) reference toBeMoved

besidesAll :: Transformable e => V2 Double -> [Reactive e a] -> Reactive e [a]
besidesAll dir = atopAllReactives . placedBesidesTo dir

-- kinda like <*>, but with a flowing direction (V2 Double)
attach :: Transformable e => V2 Double -> Reactive e (a -> b) -> Reactive e a -> Reactive e b
attach dir = besidesTo dir ($)

attachLeft, attachRight, attachUp, attachDown :: Transformable e => Reactive e (a -> b) -> Reactive e a -> Reactive e b
attachLeft = attach left
attachRight = attach right
attachUp = attach up
attachDown = attach down

infixl 4 `attachLeft`
infixl 4 `attachRight`
infixl 4 `attachUp`
infixl 4 `attachDown`

atopAllReactives :: [Reactive e a] -> Reactive e [a]
atopAllReactives = foldr (atopReactives (:)) (constant [] empty)

separatedBy :: Transformable e => V2 Double -> Form -> [Reactive e a] -> Reactive e [a]
separatedBy dir seperator [] = constant [] empty
separatedBy dir seperator (x:xs) =
    atopAllReactives (placedBesidesTo dir withSeperators)
  where
    withSeperators = x : map (attachFormTo (-dir) seperator) xs

onVisual :: (Form -> Form) -> Reactive e a -> Reactive e a
onVisual change reactive = reactive { visual = change (visual reactive) }

modifyEventPositions :: Transformable e => M33 Double -> Reactive e a -> Reactive e a
modifyEventPositions matrix reactive =
    reactive { react = react reactive . transformBy matrix }

instance Functor (Reactive e) where
  fmap f (Reactive reaction visual) = Reactive (f . reaction) visual

instance Applicative (Reactive e) where
  pure value = Reactive (const value) empty

  (Reactive reactFunc visual1) <*> (Reactive reactArg visual2)
    = Reactive react (atop visual1 visual2)
    where react event = reactFunc event (reactArg event)

instance Transformable e => Transformable (Reactive e a) where
  transformBy mat reactive =
    onVisual (transformBy mat) $ modifyEventPositions (inv33 mat) reactive

instance HasBorder (Reactive e a) where
  getBorder = getBorder . visual

instance Monoid a => Combinable (Reactive e a) where
  empty = Reactive.constant mempty empty
  reactiveA `atop` reactiveB = atopReactives mappend reactiveA reactiveB
