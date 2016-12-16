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
main = runReactive (move (V2 100 100) . alignHV (0, 0) . viewExpr)
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

getTextFieldString :: ActiveOr TextField String -> String
getTextFieldString (Active textField) = textFieldString textField
getTextFieldString (InActive str) = str

suggestionListReactive :: (a -> Form) -> [a] -> Reactive (Maybe a)
suggestionListReactive render ls =
  getFirst <$> (appendTo down (map reactive ls))
  where
    reactive = align right 0 . fmap First . suggestionReactive render

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
  | TypeFunc [RecordFieldTypeModel] TypeModel

data Type
  = IntType
  | BoolType
  | UnitType

data RecordFieldTypeModel
  = RecordFieldType TypeModel (ActiveOr TextField String)

heavyAsterisk :: Form
heavyAsterisk =
  text (dFont { textColor = grey, fontFamily = "Sans Serif" }) "✱"

renderTypeConst :: TextStyle -> Type -> Form
renderTypeConst style typ = text style (typeConstToString typ)
  where
    typeConstToString IntType = "Int"
    typeConstToString BoolType = "Bool"
    typeConstToString UnitType = "Unit"

renderUnkownType :: TextStyle -> Form
renderUnkownType style = text style "?"

renderTypeFunc :: TextStyle -> (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
renderTypeFunc style combine argsReactive resReactive =
    Reactive.besidesTo right combine
      (Reactive.attachFormTo right
        (centeredHV funcArrow)
        (centeredHV argsReactive))
      (centeredHV resReactive)
  where
    funcArrow = text style " → "

renderRecordTypes :: TextStyle -> [Reactive a] -> Reactive [a]
renderRecordTypes style reactives =
  Reactive.separatedBy right
    (gap 10 10)
    (map centeredHV reactives)

renderRecordFieldType :: (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
renderRecordFieldType combine typeReactive nameReactive =
  Reactive.besidesTo down combine
    (centeredHV typeReactive)
    (centeredHV nameReactive)


viewExpr :: ExprModel -> Reactive ExprModel
viewExpr (ValueHole typeModel Nothing) =
    separator ValueHole isTypeOfSeparator
      typeReactive
      holeReactive
  where
    typeReactive = viewType typeModel
    holeReactive =
      Reactive.constant Nothing holeWithTypeIndicatorForm

    holeWithTypeIndicatorForm =
      grayPadBorder
        (renderTypeIndicator
          (dFont { textColor = gray })
          typeModel)

renderTypeIndicator :: TextStyle -> TypeModel -> Form
renderTypeIndicator style (TypeConst typeC) = renderTypeConst style typeC
renderTypeIndicator style (TypeHole _) = renderUnkownType style
renderTypeIndicator style (TypeFunc args res) =
    Reactive.visual (renderTypeFunc style mappend renderedArgs renderedRes)
  where
    renderedRes = Reactive.irreactive (renderTypeIndicator style res)
    renderedArgs = Reactive.irreactive $ visual $
      renderRecordTypes style (map renderField args)
    renderField (RecordFieldType typeModel txtField) =
      renderRecordFieldType mappend
        (Reactive.irreactive (renderTypeIndicator style typeModel))
        (Reactive.irreactive (text style (getTextFieldString txtField)))


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
      , TypeConst UnitType
      , TypeFunc
          [ RecordFieldType (TypeHole Nothing) (InActive "arg1")
          , RecordFieldType (TypeHole Nothing) (InActive "arg2")
          ]
          (TypeHole Nothing)
      ]

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

viewType (TypeFunc args res) =
    renderTypeFunc dFont TypeFunc
      argsReactive
      (viewType res)
  where
    argsReactive = renderRecordTypes dFont (map viewRecordField args)

viewType (TypeConst typ) =
    Reactive.constant (TypeConst typ) (renderTypeConst dFont typ)


viewRecordField :: RecordFieldTypeModel -> Reactive RecordFieldTypeModel
viewRecordField (RecordFieldType typeModel textField) =
  renderRecordFieldType RecordFieldType
    (viewType typeModel)
    (viewFocusableTextField dFont textField)

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
