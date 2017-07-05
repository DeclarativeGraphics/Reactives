module Components.TextField where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys
import Linear
import Utils (orElse)
import FormUtils

import Component
import qualified Event

makeHidden :: String -> Component String
makeHidden text =
  mapValue toString (make False (caretAt 0 text))

make :: Bool -> TextField -> Component TextField
make focused value = Component
    { dispatch = make focused . nextState
    , render = (if focused then addBorder blue else id) render
    , value = value
    , focusEvent = \newFocus -> make newFocus value
    , focused = focused
    }
  where
    nextState event =
      if focused then
        Event.handleChain
          [ Event.mousePress (Event.buttonGuard MBLeft handleClick)
          , Event.keyPress (Event.keyGuard KeyLeft caretLeft)
          , Event.keyPress (Event.keyGuard KeyRight caretRight)
          , Event.keyPress (Event.keyGuard KeyBackspace deleteLeft)
          , Event.keyPress (Event.keyGuard KeyDelete deleteRight)
          , Event.textInput addTextFieldText ]
          event
          value
      else
        value
    handleClick (V2 x y) textFieldValue =
      moveCaretToPixel x (caretAt 0 (toString textFieldValue))
    render =
      if focused
        then renderWithCaret defaultTextStyle value
        else text defaultTextStyle (toString value)


renderWithCaret :: TextStyle -> TextField -> Form
renderWithCaret textStyle tf@(TextField leftFromCaret rightFromCaret) =
  collapseBorder
    (appendTo right
      [ Bordered (getBorder textUntilCaret) empty
      , alignHV (0, 0) caret ])
  `atop` text textStyle (toString tf)
  where
    textUntilCaret = text textStyle (reverse leftFromCaret)
    caretHeight = graphicHeight (text textStyle "|")
    caret = filled black (rectangle (caretHeight * 0.05) caretHeight)

moveCaretToPixel :: Double -> TextField -> TextField
moveCaretToPixel pixelX textField
  | pixelX <= acceptingPoint = textField
  | otherwise =
    fmap (moveCaretToPixel pixelX) (maybeCaretRight textField)
    `orElse`
    textField
  where
    acceptingPoint = (widthWithoutNext + widthWithNext) / 2
    widthWithoutNext = graphicWidth (text defaultTextStyle (beforeCaret textField))
    widthWithNext    = graphicWidth (text defaultTextStyle (beforeCaret (caretRight textField)))

data TextField
  = TextField
  { beforeCaret :: String
  , afterCaret :: String
  } deriving (Show, Eq)

toString :: TextField -> String
toString (TextField beforeCaret afterCaret) = reverse beforeCaret ++ afterCaret

caretLeft :: TextField -> TextField
caretLeft (TextField (c:left) right) = TextField left (c:right)
caretLeft tf = tf

caretRight :: TextField -> TextField
caretRight tf = maybeCaretRight tf `orElse` tf

maybeCaretRight :: TextField -> Maybe TextField
maybeCaretRight (TextField left (c:right)) = Just (TextField (c:left) right)
maybeCaretRight _ = Nothing

deleteLeft :: TextField -> TextField
deleteLeft (TextField (c:left) right) = TextField left right
deleteLeft tf = tf

deleteRight :: TextField -> TextField
deleteRight (TextField left (c:right)) = TextField left right
deleteRight tf = tf

addTextFieldText :: String -> TextField -> TextField
addTextFieldText str (TextField left right) = TextField (reverse str ++ left) right

caretAt :: Int -> String -> TextField
caretAt n str = TextField (reverse (take n str)) (drop n str)
