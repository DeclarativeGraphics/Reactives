module Reactive where

import Graphics.Declarative.Transforms
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import Control.Lens
import Utils (orElse, orTry, isInside, leftAngle, rightAngle)
import Control.Monad (join, liftM2, mplus)

import Linear

data Reactive event model
  = Reactive
  { react :: event -> Maybe model
  , visual :: Form
  }

type Pos = (Double, Double)

static :: Form -> Reactive e a
static form = Reactive
  { react = const Nothing
  , visual = form
  }

withEvent :: (e -> Maybe a -> Maybe b) -> Reactive e a -> Reactive e b
withEvent f reactive =
    Reactive (\event -> f event (react reactive event)) (visual reactive)

onEvent :: (e -> Maybe a) -> Reactive e a -> Reactive e a
onEvent handler =
  withEvent (\event answerBelow -> answerBelow `mplus` handler event)

onEventOverriding :: (e -> Maybe a) -> Reactive e a -> Reactive e a
onEventOverriding handler =
  withEvent (\event answerBelow -> handler event `mplus` answerBelow)

processEvent :: (a -> Maybe b) -> Reactive e a -> Reactive e b
processEvent handler =
  withEvent (\_ answerBelow -> handler =<< answerBelow)

viewList :: (model -> Reactive e [model]) -> [model] -> [Reactive e [model]]
viewList viewItem models =
  zipWith (\index item -> fmap (updateAt index) (viewItem item)) [0..] models
  where
    updateAt index replacements = concat (set (element index) replacements (map return models))

-- wrapFilterOutsideEvents :: (m -> Reactive Input m) -> m -> Reactive Input m
-- wrapFilterOutsideEvents view model =
--     Reactive.onEvent handleEvent innerReactive
--   where
--     innerReactive = view model
--     handleEvent event newModel
--       | eventInside innerReactive event = newModel
--       | otherwise = model

eventInside :: HasBorder a => a -> Input -> Bool
eventInside sth (MouseInput m) = isInside sth (view mouseInputPos m)
eventInside sth _ = True

onVisual :: (Form -> Form) -> Reactive e a -> Reactive e a
onVisual change reactive = reactive { visual = change (visual reactive) }

modifyEventPositions :: Transformable e => M33 Double -> Reactive e a -> Reactive e a
modifyEventPositions matrix reactive =
    reactive { react = react reactive . transformBy matrix }

instance Functor (Reactive e) where
  fmap f (Reactive reaction visual) = Reactive (fmap f . reaction) visual

instance Transformable e => Transformable (Reactive e a) where
  transformBy mat reactive =
    onVisual (transformBy mat) $ modifyEventPositions (inv33 mat) reactive

instance HasBorder (Reactive e a) where
  getBorder = getBorder . visual

instance Semigroup (Reactive e a) where
  reactiveAbove <> reactiveBelow = Reactive
    { react = \event -> react reactiveAbove event `mplus` react reactiveBelow event
    , visual = visual reactiveAbove <> visual reactiveBelow
    }

instance Monoid (Reactive e a) where
  mempty = Reactive.static mempty
