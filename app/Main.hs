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
import Data.Monoid (First(..))
import Utils (orElse, isInside, orTry, rightAngle)
import Linear
import FormUtils

main :: IO ()
main = runReactive (move (V2 100 100) . viewExpr)
  (ValueHole (TypeHole Nothing) Nothing)

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
            | not (isInside activeReactive pos) =
              InActive . makeInActive
          maybeMakeInactive event = Active
       in Reactive.onEvent maybeMakeInactive activeReactive
    (InActive inActive) ->
      let inActiveReactive = viewInActive inActive
          maybeMakeActive (MouseInput (MousePress pos MBLeft))
            | isInside inActiveReactive pos = Active . makeActive
          maybeMakeActive event                  = InActive
       in Reactive.onEvent maybeMakeActive inActiveReactive

viewFocusableTextField :: TextStyle -> ActiveOr TextField String -> Reactive (ActiveOr TextField String)
viewFocusableTextField style =
  viewActivatable
    textFieldString
    (\str -> textField str "")
    (viewTextField style)
    (Reactive.fromModel (text style))

suggestionListReactive :: (a -> Form) -> [a] -> Reactive (Maybe a)
suggestionListReactive render ls =
  getFirst <$> (appendTo down (map reactive ls))
  where
    reactive = fmap First . suggestionReactive render

suggestionReactive :: (a -> Form) -> a -> Reactive (Maybe a)
suggestionReactive render value =
    Reactive.onEvent eventHandler reactive
  where
    reactive = Reactive.constant Nothing (render value)

    eventHandler =
      Event.mousePress
        (Event.buttonGuard MBLeft
          (Event.insideGuard reactive (const (Just value))))

data ExprModel
  = ValueHole TypeModel (Maybe [ExprModel])

data TypeModel
  = TypeConst Type
  | TypeHole (Maybe [TypeModel])

data Type
  = IntType
  | BoolType
  | UnitType
  | FunType Type Type
  | Unkown

extractType :: TypeModel -> Type
extractType (TypeConst typ) = typ
extractType (TypeHole _) = Unkown

heavyAsterisk :: Form
heavyAsterisk =
  text (dFont { textColor = grey, fontFamily = "Sans Serif" }) "✱"

viewExpr :: ExprModel -> Reactive ExprModel
viewExpr (ValueHole typeModel Nothing) =
    separator ValueHole isTypeOfSeparator
      typeReactive
      holeReactive
  where
    typeReactive = viewType typeModel
    holeReactive =
      Reactive.constant Nothing typeForm

    typeForm =
      grayPadBorder
        (renderType
          (dFont { textColor = gray })
          (extractType typeModel))

viewType :: TypeModel -> Reactive TypeModel
viewType (TypeHole Nothing) =
    Reactive.onEvent eventHandler reactive
  where
    reactive = Reactive.constant (TypeHole Nothing) render

    render = grayPadBorder heavyAsterisk

    eventHandler =
      Event.mousePress
        (Event.buttonGuard MBLeft
          (Event.insideGuard reactive (const (TypeHole (Just suggestionList)))))

    suggestionList =
      [ TypeConst IntType
      , TypeConst BoolType
      , TypeConst UnitType ]

viewType (TypeHole (Just list)) =
    Reactive.onVisual grayPadBorder reactive
  where
    reactive = (`orElse` (TypeHole (Just list))) <$> suggestionsReactive

    suggestionsReactive =
      Reactive.besidesTo down orTry
        holeReactive
        (suggestionListReactive (visual . viewType) list)

    holeReactive =
      Reactive.onEvent
        (Event.mousePress
          (Event.buttonGuard MBLeft
            (Event.insideGuard holeReactive (const (Just (TypeHole Nothing))))))
        (Reactive.constant Nothing heavyAsterisk)

viewType (TypeConst typ) =
    Reactive.constant (TypeConst typ) (renderType dFont typ)

renderType :: TextStyle -> Type -> Form
renderType style typ = text style (typeToString typ)
  where
    typeToString IntType = "Int"
    typeToString BoolType = "Bool"
    typeToString UnitType = "()"
    typeToString Unkown = "?"
    typeToString (FunType arg res) =
      typeToString arg ++ " -> " ++ typeToString res


grayPadBorder :: Form -> Form
grayPadBorder = addBorder gray . padded 4

isTypeOfSeparator :: Double -> Double -> Form
isTypeOfSeparator spanLeft spanRight =
    padded 4 $
      outlined (solid black) $
        Bordered
          (Border.fromBoundingBox (vecLeft, vecRight + sepDir))
          (openPath $
            pp (vecLeft + sepDir) `lineConnect`
            pp vecLeft `lineConnect`
            pp vecRight `lineConnect`
            pp (vecRight + sepDir))
  where
    toTuple (V2 x y) = (x, y)
    pp = pathPoint . toTuple

    sepDir = down ^* 5
    vecLeft = left ^* spanLeft
    vecRight = right ^* spanRight
