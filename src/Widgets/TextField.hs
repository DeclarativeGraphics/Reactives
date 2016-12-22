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

data TextField = TextField String String deriving (Show, Eq, Read)

construct :: String -> String -> TextField
construct leftFromCaret rightFromCaret =
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

add :: String -> TextField -> TextField
add str (TextField left right) = TextField (reverse str ++ left) right

toString :: TextField -> String
toString (TextField left right) = reverse left ++ right

view :: TextStyle -> TextField -> Reactive TextField
view textStyle =
    Reactive.onEvent eventHandler
  . Reactive.fromModel (render textStyle)
  where
    eventHandler =
      Event.handleChain
        [ Event.keyPress (Event.keyGuard KeyLeft caretLeft)
        , Event.keyPress (Event.keyGuard KeyRight caretRight)
        , Event.keyPress (Event.keyGuard KeyBackspace deleteLeft)
        , Event.keyPress (Event.keyGuard KeyDelete deleteRight)
        , Event.textInput add ]

render :: TextStyle -> TextField -> Form
render textStyle tf@(TextField leftFromCaret rightFromCaret) =
  collapseBorder
    (appendTo right
      [ Bordered (getBorder textUntilCaret) empty
      , alignHV (0, 0) caret ])
  `atop` text textStyle (toString tf)
  where
    textUntilCaret = text textStyle (reverse leftFromCaret)
    caretHeight = graphicHeight (text textStyle "|")
    caret = filled black (rectangle (caretHeight*0.05) caretHeight)

viewActivatable :: TextStyle -> ActiveOr TextField String -> Reactive (ActiveOr TextField String)
viewActivatable style =
  Activatable.view
    toString
    (\str -> construct str "")
    (view style)
    (Reactive.fromModel (text style))

extractString :: ActiveOr TextField String -> String
extractString (Active text) = toString text
extractString (InActive str) = str
