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
main = runReactive (move (V2 100 100) . moCounter) (0, False)

dFont :: TextStyle
dFont = font "monospace" 30



pairTemplate :: Reactive a -> Reactive b -> Reactive (a, b)
pairTemplate fstReactive sndReactive =
  centeredHV $
    Reactive.besidesTo right (,)
      (Reactive.attachFormTo right comma fstReactive)
      sndReactive
  where comma = text dFont ","

viewPair :: (Int, Int) -> Reactive (Int, Int)
viewPair (mLeft, mRight) =
  move (V2 200 200) $ rotateRad (fromIntegral (mLeft + 10 * mRight) * pi / 180) $
    pairTemplate (viewCounter mLeft) (viewCounter mRight)




listTemplate :: [Reactive a] -> Reactive [a]
listTemplate reactives =
    Reactive.attachFormTo left leftBracket $
      Reactive.attachFormTo right rightBracket $
        Reactive.seperatedBy right comma reactives
  where
    leftBracket = text dFont "["
    rightBracket = text dFont "]"
    comma = text dFont ", "

viewList :: [Int] -> Reactive [Int]
viewList = move (V2 100 100) . listTemplate . map viewCounter


addMouseOver :: (Bool -> a -> Reactive (Maybe a)) -> (a, Bool) -> Reactive (Maybe (a, Bool))
addMouseOver innerView (model, mouseIsOver)
  = Reactive.postEvent
      (\maybeNewModel -> Reactive.mouseMove (handleMouseMove maybeNewModel))
      innerReactive
  where
    innerReactive = innerView mouseIsOver model
    handleMouseMove maybeNewModel pos =
      (maybeNewModel `orElse` model, Reactive.isInside innerReactive pos)

moCounter :: (Int, Bool) -> Reactive (Int, Bool)
moCounter = optionalUpdate (addMouseOver counterMouseOver)

counterMouseOver :: Bool -> Int -> Reactive (Maybe Int)
counterMouseOver mouseIsOver model
  = Reactive.attachFormTo right (text dFont (show mouseIsOver))
  $ viewCounterMaybe model



viewCounter :: Int -> Reactive Int
viewCounter = optionalUpdate viewCounterMaybe

viewCounterMaybe :: Int -> Reactive (Maybe Int)
viewCounterMaybe number
  = Reactive.andFilterOutside
  $ Reactive.andOnEvent (Reactive.mouseMove (const (number + 1)))
  $ Reactive.onEvent (Reactive.click MBLeft (number + 1))
  $ Reactive.constant Nothing (text dFont (show number))
