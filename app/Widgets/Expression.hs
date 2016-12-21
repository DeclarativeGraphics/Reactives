module Widgets.Expression where

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

import qualified Widgets.TextField as TextField
import Widgets.TextField (TextField(..))
import qualified Widgets.Activatable as Activatable
import Widgets.Activatable (ActiveOr(..))


dFont :: TextStyle
dFont = font "monospace" 18

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


view :: ExprModel -> Reactive ExprModel
view (ValueHole typeModel Nothing) =
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
        (Reactive.irreactive
          (text style (TextField.extractString txtField)))


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
    (TextField.viewActivatable dFont textField)

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
