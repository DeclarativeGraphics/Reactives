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
import qualified Event
import RunReactive (runReactive)
import Utils (orElse)
import Linear

main :: IO ()
--main = runReactive (move (V2 100 100) . viewList) [0, 1, 2, 3, 4]
--main = runReactive (move (V2 100 100) . moCounter) (0, False)
--main = runReactive (move (V2 100 100) . viewCounter) 0
--main = runReactive (move (V2 100 100) . viewPair) (0, 1)
main = runReactive (move (V2 100 100) . viewTextField defaultTextStyle) (textField "Hel" "lo, World!")

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
      (Event.mouseMove handleMouseMove)
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
  = Reactive.wrapFilterOutsideEvents
  $ Reactive.onEvent eventHandler
  . Reactive.fromModel (text dFont . show)
  where
    eventHandler =
      Event.mouseMove (\pos -> (+1))
      `Event.handleAfter`
      Event.mousePress
        (Event.buttonGuard MBLeft (\pos -> (+1)))


data TextField = TextField String String deriving (Show)

textField :: String -> String -> TextField
textField leftFromCaret rightFromCaret =
  TextField (reverse leftFromCaret) rightFromCaret

caretLeft :: TextField -> TextField
caretLeft (TextField (c:left) right) = TextField left (c:right)
caretLeft tf = tf

caretRight :: TextField -> TextField
caretRight (TextField left (c:right)) = TextField (c:left) right
caretRight tf = tf

deleteLeft :: TextField -> TextField
deleteLeft (TextField (c:left) right) = TextField left right
deleteLeft tf = tf

deleteRight :: TextField -> TextField
deleteRight (TextField left (c:right)) = TextField left right
deleteRight tf = tf

addText :: String -> TextField -> TextField
addText str (TextField left right) = TextField (reverse str ++ left) right

viewTextField :: TextStyle -> TextField -> Reactive TextField
viewTextField textStyle =
    Reactive.onEvent eventHandler
  . Reactive.fromModel renderTextField
  where
    eventHandler =
      Event.handleChain
        [ Event.keyPress (Event.keyGuard KeyLeft caretLeft)
        , Event.keyPress (Event.keyGuard KeyRight caretRight)
        , Event.keyPress (Event.keyGuard KeyBackspace deleteLeft)
        , Event.keyPress (Event.keyGuard KeyDelete deleteRight)
        , Event.textInput addText ]

    renderTextField (TextField leftFromCaret rightFromCaret) =
      appendTo right
        [ text textStyle (reverse leftFromCaret)
        , collapseBorder $ alignHV (0, 0) caret
        , text textStyle rightFromCaret ]

    caretHeight = graphicHeight (text textStyle "|")
    caret = filled black (rectangle (caretHeight*0.05) caretHeight)
