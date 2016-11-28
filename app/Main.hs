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

main :: IO ()
main = runReactive viewPair (5, 10)

viewPair :: (Int, Int) -> Reactive (Int, Int)
viewPair (mLeft, mRight) =
  ((,) <$> counterL) <*> (comma *> counterR)
  where
    comma = Reactive.moveR (10, 0) $ Reactive.unit (text defaultTextStyle ",")
    counterL = viewCounter mLeft
    counterR = Reactive.moveR (20, 0) $ viewCounter mRight

viewCounter :: Int -> Reactive Int
viewCounter = optionalUpdate viewCounterMaybe

viewCounterMaybe :: Int -> Reactive (Maybe Int)
viewCounterMaybe number
  = Reactive.joinFilters
  $ Reactive.eventInside
  $ Reactive.onEvent (Reactive.click MBLeft $ number + 1)
  $ Reactive.constant Nothing (text defaultTextStyle (show number))
