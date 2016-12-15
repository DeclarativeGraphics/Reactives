module Utils where

import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Bordered

import Linear
import Data.Monoid (First(..), getFirst)

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing x = x

orTry :: Maybe a -> Maybe a -> Maybe a
orTry a b = getFirst $ mappend (First a) (First b)

isInside :: HasBorder a => a -> V2 Double -> Bool
isInside sth pos =
  purelyPositive (pos - topLeft) &&
  purelyPositive (bottomRight - pos)
  where
    (topLeft, bottomRight) =
      Border.getBoundingBox $ getBorder sth
    purelyPositive (V2 x y) = x >= 0 && y >= 0


rightAngle :: Num a => V2 a -> V2 a
rightAngle (V2 x y) = V2 (-y) x

leftAngle :: Num a => V2 a -> V2 a
leftAngle (V2 x y) = V2 y (-x)
