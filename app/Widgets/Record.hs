module Widgets.Record where

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

type Error = String

data FocusableList a = Zipper [a] a [a]

data RecordModel
  = Record [RecordFieldModel]
  deriving (Show, Eq)

data RecordModelFocused
  = RecordFocused [RecordFieldModel] RecordFieldModelFocused [RecordFieldModel]
  deriving (Show, Eq)

data RecordFieldModel
  = RecordField String
  deriving (Show, Eq)

data RecordFieldModelFocused
  = RecordFieldFocused TextField
  deriving (Show, Eq)

viewRecordFocused :: [Reactive RecordFieldModel] -> Reactive RecordFieldModelFocused -> [Reactive RecordFieldModel] -> Reactive RecordModelFocused
viewRecordFocused leftOf viewedFocus rightOf =
    Reactive.besidesTo right ($)
      (Reactive.besidesTo right RecordFocused viewedLeftOf viewedFocus)
      viewedRightOf
  where
    attachComma dir = Reactive.attachFormTo dir (text defaultTextStyle ", ")
    fieldsBesides dir fields = Reactive.besidesAll dir (map (attachComma (-dir)) fields)
    viewedLeftOf = fieldsBesides left leftOf
    viewedRightOf = fieldsBesides right rightOf

viewFocused :: RecordModelFocused -> Reactive RecordModelFocused
viewFocused (RecordFocused leftOf focus rightOf) =
  handleAddRecord
    (viewRecordFocused
      (viewField <$> leftOf)
      (viewFieldFocused focus)
      (viewField <$> rightOf))

viewField :: RecordFieldModel -> Reactive RecordFieldModel
viewField (RecordField fieldName) =
  RecordField <$>
    (Reactive.attachFormTo right
      (text defaultTextStyle " =")
      (Reactive.fromModel (text defaultTextStyle) fieldName))

viewFieldFocused :: RecordFieldModelFocused -> Reactive RecordFieldModelFocused
viewFieldFocused (RecordFieldFocused textField) =
  RecordFieldFocused <$>
    (Reactive.attachFormTo right
      (text defaultTextStyle " =")
      (TextField.view defaultTextStyle textField))

handleAddRecord :: Reactive RecordModelFocused -> Reactive RecordModelFocused
handleAddRecord =
  Reactive.onEvent
    (Event.keyPress
      (Event.keyGuard Keys.KeyReturn addNewRecordField))

addNewRecordField :: RecordModelFocused -> RecordModelFocused
addNewRecordField (RecordFocused leftOf (RecordFieldFocused focus) rightOf) =
  RecordFocused (RecordField (TextField.toString focus) : leftOf) newField rightOf
  where
    newField = RecordFieldFocused (TextField.construct "" "")
