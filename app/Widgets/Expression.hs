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
  | ValueConst Const
  deriving (Show, Eq, Read)

data TypeModel
  = TypeConst Type
  | TypeHole (Maybe [TypeModel])
  | TypeFunc [RecordFieldTypeModel] TypeModel
  deriving (Show, Eq, Read)

data Type
  = IntType
  | BoolType
  | UnitType
  deriving (Show, Eq, Read)

data Const
  = IntConst Int
  | BoolConst Bool
  | UnitConst ()
  deriving (Show, Eq, Read)

data RecordFieldTypeModel
  = RecordFieldType TypeModel (ActiveOr TextField String)
  deriving (Show, Eq, Read)

heavyAsterisk :: Form
heavyAsterisk =
  text (dFont { textColor = grey, fontFamily = "Sans Serif" }) "✱"

data RenderConf
  = RenderConf
  { textStyle :: TextStyle
  , groupingDepth :: Int
  , groupingStyle :: GroupStyle
  }

data GroupStyle = FillGroups | FrameGroups

typeRenderConf, exprRenderConf :: RenderConf
typeRenderConf
  = RenderConf
  { textStyle = dFont
  , groupingDepth = 0
  , groupingStyle = FillGroups
  }
exprRenderConf
  = RenderConf
  { textStyle = dFont { textColor = gray }
  , groupingDepth = 0
  , groupingStyle = FrameGroups
  }

deeper :: RenderConf -> RenderConf
deeper conf = conf { groupingDepth = groupingDepth conf + 1 }

toplevel :: RenderConf -> RenderConf
toplevel conf = conf { groupingDepth = 0 }

majorColor :: RenderConf -> RGB
majorColor = textColor . textStyle


renderHole :: Form -> Form
renderHole = addBackground white . padded 2 . grayPadBorder

renderConstant :: RenderConf -> Const -> Form
renderConstant conf (IntConst i) = text (textStyle conf) (show i)
renderConstant conf (BoolConst b) = text (textStyle conf) (show b)
renderConstant conf (UnitConst ()) = text (textStyle conf) "()"

renderTypeConst :: RenderConf -> Type -> Form
renderTypeConst conf typ = text (textStyle conf) (typeConstToString typ)
  where
    typeConstToString IntType = "Int"
    typeConstToString BoolType = "Bool"
    typeConstToString UnitType = "Unit"

renderUnkownType :: RenderConf -> Form
renderUnkownType conf = text (textStyle conf) "?"

renderTypeFunc :: RenderConf -> (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
renderTypeFunc conf combine argsReactive resReactive =
    Reactive.besidesTo right combine
      (Reactive.attachFormTo right
        (centeredHV funcArrow)
        (centeredHV argsReactive))
      (centeredHV resReactive)
  where
    funcArrow = text (textStyle conf) " → "

renderRecordTypes :: RenderConf -> [Reactive a] -> Reactive [a]
renderRecordTypes conf reactives =
  Reactive.separatedBy right
    (padded 3 (filled (majorColor conf) (circle 2)))
    (map centeredHV reactives)

renderRecordFieldType :: RenderConf -> (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
renderRecordFieldType conf combine typeReactive nameReactive =
  separator combine (isTypeOfSeparator (majorColor conf))
    (centeredHV typeReactive)
    (centeredHV nameReactive)

renderGrouping :: RenderConf -> Reactive a -> Reactive a
renderGrouping conf reactive =
  Reactive.onVisual
    ((case groupingStyle conf of
        FillGroups -> addBackground fillColor
        FrameGroups -> addBorder gray) . padded 4)
    reactive
  where
    grayscale col = (col, col, col)
    fillColor = grayscale (0.5 + 0.3 / (1 + fromIntegral (groupingDepth conf)))

view :: ExprModel -> Reactive ExprModel
view (ValueHole typeModel Nothing) =
    separator ValueHole (isTypeOfSeparator black)
      typeReactive
      (Reactive.onEvent onClickHole holeReactive)
  where
    typeReactive = viewType typeRenderConf typeModel
    holeReactive =
      Reactive.constant Nothing holeWithTypeIndicatorForm

    holeWithTypeIndicatorForm =
      renderHole (renderTypeIndicator exprRenderConf typeModel)

    onClickHole =
      Event.mousePress
        (Event.buttonGuard MBLeft
          (Event.insideGuard holeReactive
            (const (valueSuggestions typeModel))))

view (ValueHole typeModel (Just list)) =
  separator combine (isTypeOfSeparator black)
    typeReactive
    suggestionsReactive
  where
    typeReactive = viewType typeRenderConf typeModel

    combine newTypeModel (Just newExpr) = newExpr
    combine newTypeModel Nothing
      | newTypeModel == typeModel = ValueHole newTypeModel (Just list)
      | otherwise                 = ValueHole newTypeModel Nothing

    suggestionsReactive =
      Reactive.onVisual renderHole
        (Reactive.besidesTo down orTry
          holeReactive
          (suggestionListReactive (visual . view) list))

    holeReactive =
      Reactive.onEvent
        (Event.mousePress
          (Event.buttonGuard MBLeft
            (Event.insideGuard holeReactive
              (const (Just (ValueHole typeModel Nothing))))))
        (Reactive.constant Nothing (renderTypeIndicator exprRenderConf typeModel))

view (ValueConst c) =
  ValueConst <$> Reactive.fromModel (renderConstant exprRenderConf) c

renderTypeIndicator :: RenderConf -> TypeModel -> Form
renderTypeIndicator conf (TypeConst typeC) = renderTypeConst conf typeC
renderTypeIndicator conf (TypeHole _)      = renderUnkownType conf
renderTypeIndicator conf (TypeFunc args res) =
    Reactive.visual (renderTypeFunc conf mappend renderedArgs renderedRes)
  where
    renderedRes = Reactive.irreactive (renderTypeIndicator conf res)
    renderedArgs = Reactive.irreactive $ visual $
      renderRecordTypes conf (map renderField args)
    renderField (RecordFieldType typeModel txtField) =
      renderRecordFieldType conf mappend
        (Reactive.irreactive (renderTypeIndicator (deeper conf) typeModel))
        (Reactive.irreactive
          (text (textStyle conf) (TextField.extractString txtField)))

valueSuggestions :: TypeModel -> Maybe [ExprModel]
valueSuggestions (TypeConst IntType) = Just $ map (ValueConst . IntConst) [0, 1]
valueSuggestions (TypeConst BoolType) = Just $ map (ValueConst . BoolConst) [True, False]
valueSuggestions (TypeConst UnitType) = Just [ValueConst (UnitConst ())]
valueSuggestions _ = Nothing


viewType :: RenderConf -> TypeModel -> Reactive TypeModel
viewType conf (TypeHole Nothing) =
    Reactive.onEvent eventHandler reactive
  where
    reactive = Reactive.constant (TypeHole Nothing) render

    render = renderHole heavyAsterisk

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

viewType conf (TypeHole (Just list)) =
    (`orElse` (TypeHole (Just list))) <$> suggestionsReactive
  where
    suggestionsReactive =
      Reactive.onVisual renderHole
        (Reactive.besidesTo down orTry
          holeReactive
          (suggestionListReactive
            (visual . viewType (toplevel conf)) list))

    holeReactive =
      Reactive.onEvent
        (Event.mousePress
          (Event.buttonGuard MBLeft
            (Event.insideGuard holeReactive (const (Just (TypeHole Nothing))))))
        (Reactive.constant Nothing heavyAsterisk)

viewType conf (TypeFunc args res) =
    renderTypeFunc conf TypeFunc
      argsReactive
      (viewType conf res)
  where
    argsReactive = renderRecordTypes conf (map (viewRecordField conf) args)

viewType conf (TypeConst typ) =
    Reactive.constant (TypeConst typ) (renderTypeConst conf typ)


viewRecordField :: RenderConf -> RecordFieldTypeModel -> Reactive RecordFieldTypeModel
viewRecordField conf (RecordFieldType typeModel textField) =
  renderRecordFieldType conf RecordFieldType
    (viewType (deeper conf) typeModel)
    (TextField.viewActivatable (textStyle conf) textField)

grayPadBorder :: Form -> Form
grayPadBorder = addBorder gray . padded 4

isTypeOfSeparator :: RGB -> Double -> Double -> Form
isTypeOfSeparator color spanLeft spanRight =
    padded 4 $
      outlined (solid color) $
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
