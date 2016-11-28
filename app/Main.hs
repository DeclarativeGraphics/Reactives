module Main where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import qualified Reactive
import Reactive (Reactive(..))
import RunReactive (optionalUpdate, runReactive)
import Utils (orElse)

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)

main :: IO ()
main = runReactive viewPair (5, 10)

infixl 4 <*->
(<*->) :: Reactive (a -> b) -> Reactive a -> Reactive b
left <*-> right = left <*> rightMoved
  where
    leftBorder = getBorder $ visual left
    rightBorder = getBorder $ visual right
    displacement = displacementTo Vec2.right leftBorder rightBorder
    rightMoved = Reactive.moveR displacement right

infixl 5 *->
(*->) :: Reactive b -> Reactive a -> Reactive a
left *-> right = pure (const id) <*> left <*-> right

viewPair :: (Int, Int) -> Reactive (Int, Int)
viewPair (mLeft, mRight) =
  (,) <$> counterL <*-> comma *-> counterR
  where
    comma = Reactive.moveR (10, 0) $ Reactive.unit (text defaultTextStyle ",")
    counterL = viewCounter mLeft
    counterR = Reactive.moveR (20, 0) $ viewCounter mRight

viewCounter :: Int -> Reactive Int
viewCounter = optionalUpdate viewCounterMaybe

viewCounterMaybe :: Int -> Reactive (Maybe Int)
viewCounterMaybe number
  = Reactive.andFilterOutside
  $ Reactive.onEvent (Reactive.click MBLeft (number + 1))
  $ Reactive.constant Nothing (text defaultTextStyle (show number))
