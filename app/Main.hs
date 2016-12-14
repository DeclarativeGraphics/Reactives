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
import FormUtils

main :: IO ()
main = runReactive (move (V2 100 100) . viewExpr)
  (EHole (TypeLit (InActive "Form")))

dFont :: TextStyle
dFont = font "monospace" 18

data TextField = TextField String String

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

textFieldString :: TextField -> String
textFieldString (TextField left right) = reverse left ++ right

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

    renderTextField tf@(TextField leftFromCaret rightFromCaret) =
      collapseBorder
        (appendTo right
          [ Bordered (getBorder textUntilCaret) empty
          , alignHV (0, 0) caret ])
      `atop` text textStyle (textFieldString tf)
      where
        textUntilCaret = text textStyle (reverse leftFromCaret)

    caretHeight = graphicHeight (text textStyle "|")
    caret = filled black (rectangle (caretHeight*0.05) caretHeight)




data ActiveOr active inactive
  = Active active
  | InActive inactive

viewActivatable
  :: (act -> inact)
  -> (inact -> act)
  -> (act -> Reactive act)
  -> (inact -> Reactive inact)
  -> ActiveOr act inact
  -> Reactive (ActiveOr act inact)
viewActivatable makeInActive makeActive viewActive viewInActive activeOr =
  case activeOr of
    (Active active) ->
      let activeReactive = viewActive active
          maybeMakeInactive (MouseInput (MousePress pos MBLeft))
            | not (Reactive.isInside activeReactive pos) =
              InActive . makeInActive
          maybeMakeInactive event = Active
       in Reactive.onEvent maybeMakeInactive activeReactive
    (InActive inActive) ->
      let inActiveReactive = viewInActive inActive
          maybeMakeActive (MouseInput (MousePress pos MBLeft))
            | Reactive.isInside inActiveReactive pos = Active . makeActive
          maybeMakeActive event                  = InActive
       in Reactive.onEvent maybeMakeActive inActiveReactive

viewFocusableTextField :: TextStyle -> ActiveOr TextField String -> Reactive (ActiveOr TextField String)
viewFocusableTextField style =
  viewActivatable
    textFieldString
    (\str -> textField str "")
    (viewTextField style)
    (Reactive.fromModel (text style))


data Expr
  = EHole Type

data Type = TypeLit (ActiveOr TextField String)


viewExpr :: Expr -> Reactive Expr
viewExpr (EHole typ) =
  EHole <$>
    Reactive.onVisual
      (addBorder grey)
      (Reactive.onVisual (padded 4) (viewType grey typ))

viewType :: RGB -> Type -> Reactive Type
viewType mainColor (TypeLit text) =
    centeredHV (TypeLit <$> viewFocusableTextField font text)
  where
    font = dFont { textColor = mainColor }
