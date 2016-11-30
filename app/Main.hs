module Main where

import Graphics.Declarative.Classes
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
import Linear

main :: IO ()
main = runReactive viewPair (5, 10)

pairTemplate :: Reactive a -> Reactive b -> Reactive (a, b)
pairTemplate fstReactive sndReactive =
  centeredHV $
    Reactive.besidesTo right (,)
      (Reactive.attachFormTo right fstReactive comma)
      sndReactive
  where comma = text (font "monospace" 30) ","

viewPair :: (Int, Int) -> Reactive (Int, Int)
viewPair (mLeft, mRight) =
  move (V2 200 200) $ rotateRad (fromIntegral (mLeft + 10 * mRight) * pi / 180) $
    pairTemplate (viewCounter mLeft) (viewCounter mRight)

viewCounter :: Int -> Reactive Int
viewCounter = optionalUpdate viewCounterMaybe

viewCounterMaybe :: Int -> Reactive (Maybe Int)
viewCounterMaybe number
  = Reactive.andFilterOutside
  $ Reactive.onEvent (Reactive.click MBLeft (number + 1))
  $ Reactive.constant Nothing (text (font "monospace" 30) (show number))
