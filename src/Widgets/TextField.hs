module Widgets.TextField where

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
import qualified Widgets.Activatable as Activatable
import Widgets.Activatable (ActiveOr(..))

import Linear
import Utils (orElse)
import FormUtils

data TextField
  = TextField
  { beforeCaret :: String
  , afterCaret :: String
  } deriving (Show, Eq, Read)

type Model = ActiveOr TextField String

active :: String -> String -> TextField
active leftFromCaret rightFromCaret =
  TextField (reverse leftFromCaret) rightFromCaret

inactive :: String -> Model
inactive = Inactive

emptyActive :: Model
emptyActive = Active (TextField "" "")

emptyInactive :: Model
emptyInactive = Inactive ""


caretLeft :: TextField -> TextField
caretLeft (TextField (c:left) right) = TextField left (c:right)
caretLeft tf = tf

caretRight :: TextField -> TextField
caretRight textField = maybeCaretRight textField `orElse` textField

maybeCaretRight :: TextField -> Maybe TextField
maybeCaretRight (TextField left (c:right)) = Just (TextField (c:left) right)
maybeCaretRight _ = Nothing

deleteLeft :: TextField -> TextField
deleteLeft (TextField (c:left) right) = TextField left right
deleteLeft tf = tf

deleteRight :: TextField -> TextField
deleteRight (TextField left (c:right)) = TextField left right
deleteRight tf = tf

write :: String -> TextField -> TextField
write str (TextField left right) = TextField (reverse str ++ left) right

toString :: TextField -> String
toString (TextField left right) = reverse left ++ right

viewTextField :: TextStyle -> String -> TextField -> Reactive Input TextField
viewTextField textStyle placeholder =
    Reactive.onEvent eventHandler
  . Reactive.fromModel (render textStyle placeholder)
  where
    eventHandler =
      Event.handleChain
        [ Event.mousePress (Event.buttonGuard MBLeft handleClick)
        , Event.keyPress (Event.keyGuard KeyLeft caretLeft)
        , Event.keyPress (Event.keyGuard KeyRight caretRight)
        , Event.keyPress (Event.keyGuard KeyBackspace deleteLeft)
        , Event.keyPress (Event.keyGuard KeyDelete deleteRight)
        , Event.textInput write ]
    handleClick pos textField = calcCaret textStyle pos (toString textField)

render :: TextStyle -> String -> TextField -> Form
render textStyle placeholder tf@(TextField leftFromCaret rightFromCaret) =
    addBorder lightBlue textWithCaret
  where
    textWithCaret =
      collapseBorder
        (appendTo right
          [ Bordered (getBorder textUntilCaret) Graphics.Declarative.Classes.empty
          , alignHV (0, 0) caret ])
      `atop` renderText (toString tf)

    renderText "" = renderPlaceholder textStyle placeholder
    renderText str = text textStyle str

    textUntilCaret = text textStyle (reverse leftFromCaret)
    caretHeight = graphicHeight (text textStyle "|")
    caret = filled black (rectangle (caretHeight*0.05) caretHeight)

renderPlaceholder :: TextStyle -> String -> Form
renderPlaceholder style = text style { textColor = darkGrey }

renderStatic :: TextStyle -> String -> String -> Form
renderStatic style placeholder "" = renderPlaceholder style placeholder
renderStatic style _ str = text style str

view :: TextStyle -> String -> Model -> Reactive Input Model
view style placeholder =
  Activatable.view
    toString
    (calcCaret style)
    (viewTextField style placeholder)
    (Reactive.fromModel (renderStatic style placeholder))

calcCaret :: TextStyle -> V2 Double -> String -> TextField
calcCaret style (V2 x y) str = moveCaretToPixel style x (active "" str)

moveCaretToPixel :: TextStyle -> Double -> TextField -> TextField
moveCaretToPixel style pixelX textField
  | pixelX <= acceptingPoint = textField
  | otherwise =
    fmap (moveCaretToPixel style pixelX) (maybeCaretRight textField)
    `orElse`
    textField
  where
    acceptingPoint = (widthWithoutNext + widthWithNext) / 2
    widthWithoutNext = graphicWidth (text style (beforeCaret textField))
    widthWithNext    = graphicWidth (text style (beforeCaret (caretRight textField)))


getContent :: Model -> String
getContent (Active text) = toString text
getContent (Inactive str) = str

isActive :: Model -> Bool
isActive (Active _) = True
isActive (Inactive _) = False
