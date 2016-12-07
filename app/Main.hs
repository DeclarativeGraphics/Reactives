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
import Control.Monad ((<=<), liftM2)

main :: IO ()
main = runReactive (move (V2 100 100) . viewList) [0, 1, 2, 3, 4]
--main = runReactive (move (V2 100 100) . moCounter) (0, False)
--main = runReactive (move (V2 100 100) . viewCounter) 0

dFont :: TextStyle
dFont = font "monospace" 30

{-

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

-}


listTemplate :: [Reactive a] -> Reactive [a]
listTemplate reactives =
    Reactive.attachFormTo left leftBracket $
      Reactive.attachFormTo right rightBracket $
        Reactive.separatedBy right comma reactives
  where
    leftBracket = text dFont "["
    rightBracket = text dFont "]"
    comma = text dFont ", "

viewList :: [Int] -> Reactive [Int]
viewList = move (V2 100 100) . listTemplate . map viewCounter


addMouseOver :: (Bool -> a -> Reactive a) -> (a, Bool) -> Reactive (a, Bool)
addMouseOver innerView (model, mouseIsOver)
  = Reactive.onEvent
      (Reactive.mouseMove handleMouseMove)
      innerReactive
  where
    innerReactive = fmap (flip (,) mouseIsOver) $ innerView mouseIsOver model
    handleMouseMove pos (newModel, _) =
      (newModel, Reactive.isInside innerReactive pos)

moCounter :: (Int, Bool) -> Reactive (Int, Bool)
moCounter = addMouseOver counterMouseOver

counterMouseOver :: Bool -> Int -> Reactive Int
counterMouseOver mouseIsOver model
  = Reactive.attachFormTo right (text dFont (show mouseIsOver))
  $ viewCounter model


viewCounter :: Int -> Reactive Int
viewCounter
  = Reactive.wrapOutsideFilter
  $ Reactive.onEvent
      (Reactive.andThenEvent
        (Reactive.mouseMove (const (+1)))
        (Reactive.click MBLeft (+1)) )
  . Reactive.fromModel (text dFont . show)
