module Widgets.RecordType where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys

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

data TypeF a
  = RecordTypeF [(String, a)]
  | NumberTypeF
  | TypeHoleF
  deriving (Show, Eq)

instance Functor TypeF where
  fmap f NumberTypeF = NumberTypeF
  fmap f TypeHoleF = TypeHoleF
  fmap f (RecordTypeF assocs) = RecordTypeF (map (mapRight f) assocs)
    where
      mapRight f (x, y) = (x, f y)

data Type
  = RecordType [(String, Type)]
  | NumberType
  | TypeHole
  deriving (Show, Eq)

data TypeContext
  = Top
  | InRecordType TypeContext [(String, Type)] String [(String, Type)]
  deriving (Show, Eq)

data TypeModel = TypeModel Type TypeContext
  deriving (Show, Eq)

view :: TypeModel -> Reactive TypeModel
view model =
  Reactive.onEvent eventHandler
    (Reactive.constant model
      (cataTypeModel renderFlat markFocused model))
  where
    eventHandler =
      Event.handleChain
        [ Event.keyPress (Event.keyGuard Keys.KeyEscape goUp)
        , Event.keyPress (Event.keyGuard Keys.KeyUp goLeft)
        , Event.keyPress (Event.keyGuard Keys.KeyDown goRight)
        , Event.keyPress (Event.keyGuard Keys.KeyReturn goDown)
        ]

cataTypeModel :: (TypeF a -> a) -> (a -> a) -> TypeModel -> a
cataTypeModel transformF markFocused (TypeModel typ ctx) =
    cataContext transformF (markFocused focused) ctx
  where
    focused = markFocused (cataType transformF typ)

cataContext :: (TypeF a -> a) -> a -> TypeContext -> a
cataContext algebra focused Top = focused
cataContext algebra focused (InRecordType ctxAbove leftOf focusedName rightOf) =
    cataContext algebra (algebra (RecordTypeF assocs)) ctxAbove
  where
    assocs =
      reverse (mapValues (cataType algebra) leftOf) ++
      [(focusedName, focused)] ++
      (mapValues (cataType algebra) rightOf)

cataType :: (TypeF a -> a) -> Type -> a
cataType transformF typ = transformF (fmap (cataType transformF) (deconstructLayer typ))

deconstructLayer :: Type -> TypeF Type -- "unfix"
deconstructLayer NumberType = NumberTypeF
deconstructLayer TypeHole = TypeHoleF
deconstructLayer (RecordType assocs) = RecordTypeF assocs

goUp :: TypeModel -> TypeModel
goUp (TypeModel typ Top) = TypeModel typ Top
goUp (TypeModel typ (InRecordType ctx leftOf focusedName rightOf)) =
  TypeModel (RecordType (reverse leftOf ++ [(focusedName, typ)] ++ rightOf)) ctx

goLeft :: TypeModel -> TypeModel
goLeft (TypeModel typ Top) = TypeModel typ Top
goLeft (TypeModel typ (InRecordType ctx [] focusedName rightOf)) = TypeModel typ (InRecordType ctx [] focusedName rightOf)
goLeft (TypeModel typ (InRecordType ctx ((leftName, leftType):xs) focusedName rightOf)) =
  TypeModel leftType (InRecordType ctx xs leftName ((focusedName, typ):rightOf))

goRight :: TypeModel -> TypeModel
goRight (TypeModel typ Top) = TypeModel typ Top
goRight (TypeModel typ (InRecordType ctx leftOf focusedName [])) = TypeModel typ (InRecordType ctx leftOf focusedName [])
goRight (TypeModel typ (InRecordType ctx leftOf focusedName ((rightName, rightType):xs))) =
  TypeModel rightType (InRecordType ctx ((focusedName, typ):leftOf) rightName xs)

goDown :: TypeModel -> TypeModel
goDown (TypeModel (RecordType ((name, typ):assocs)) ctx) =
  TypeModel typ (InRecordType ctx [] name assocs)
goDown (TypeModel anythingElse ctx) = TypeModel anythingElse ctx

markFocused :: Form -> Form
markFocused originalForm = addBorder red originalForm

renderContext :: TypeContext -> Form -> Form
renderContext Top focused = focused
renderContext (InRecordType ctx leftOf focusedName rightOf) focused =
  renderContext ctx
    (appendTo down
      (renderRecordAssocs
        (reverse leftRendered ++ [(focusedName, focused)] ++ rightRendered)))
  where
    renderTypeInAssoc (name, typ) = (name, renderType typ)
    leftRendered = map renderTypeInAssoc leftOf
    rightRendered = map renderTypeInAssoc rightOf

renderFlat :: TypeF Form -> Form
renderFlat NumberTypeF = text defaultTextStyle "Number"
renderFlat TypeHoleF = text defaultTextStyle "<hole>"
renderFlat (RecordTypeF associations) =
    appendTo down (renderRecordAssocs associations)

renderType :: Type -> Form
renderType = cataType renderFlat

renderRecordAssocs :: [(String, Form)] -> [Form]
renderRecordAssocs = map renderRecordAssoc

renderRecordAssoc :: (String, Form) -> Form
renderRecordAssoc (associationName, typRendered) =
  appendTo right [text defaultTextStyle associationName, text defaultTextStyle ": ", typRendered]


mapValues :: (a -> b) -> [(k, a)] -> [(k, b)]
mapValues f = map (onSnd f)
  where
    onSnd func (x, y) = (x, func y)
