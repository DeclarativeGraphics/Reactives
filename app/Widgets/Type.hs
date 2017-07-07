module Widgets.Type where

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
import Utils
import Linear
import FormUtils
import qualified Data.Map.Lazy as Map

import qualified Widgets.TextField as TextField
import qualified Widgets.Button as Button
import qualified Widgets.DropDownList as DropDownList
import qualified Widgets.Record as Record

import qualified Backend

data Model -- for types
  = RecordType (Record.Model Model)
  | Var TextField.Model
  | Hole (DropDownList.Model Model)
  deriving (Show, Eq)

monoStyle :: TextStyle
monoStyle = defaultTextStyle { fontFamily = "monospace" }

recordSettings :: Backend.TypeEnv -> Record.Settings Model
recordSettings typeEnv = Record.Settings
  { Record.textStyle = monoStyle
  , Record.newInstance = hole
  , Record.viewInner = view typeEnv
  , Record.placeholderText = "field name"
  , Record.emptyRecord = text monoStyle "Unit"
  }

example :: Model
example
  = record
  [ ( "key", var "value" )
  , ( "record"
    , record
      [ ( "nested", var "records" )
      , ( "mutliple", var "fields" )
      , ( "all good are", var "3" )
      , ( "holes are this", hole )
      ]
    )
  ]

record :: [(String, Model)] -> Model
record = RecordType . Record.construct

var :: String -> Model
var = Var . TextField.inactive

hole :: Model
hole = Hole DropDownList.construct


typeToModel :: Backend.Type -> Model
typeToModel Backend.Nat = var "Nat"


calculateOptions :: Backend.TypeEnv -> [Model]
calculateOptions typeEnv =
  [RecordType Record.empty] ++ map typeToModel (Map.elems typeEnv)


view :: Backend.TypeEnv -> Model -> Reactive Model
view typeEnv (Var textField) = Var <$> TextField.view monoStyle "type variable" textField
view typeEnv (RecordType record) = RecordType <$> Record.view (recordSettings typeEnv) record
view typeEnv (Hole dropDownList) = viewHole typeEnv dropDownList



holeDropDownListSettings :: Backend.TypeEnv -> DropDownList.Settings Model
holeDropDownListSettings typeEnv = DropDownList.Settings
  { DropDownList.textStyle = monoStyle
  , DropDownList.buttonText = "Choose type..."
  , DropDownList.dropDownText = "Choose type:"
  , DropDownList.renderModel = Reactive.visual . view typeEnv
  }

viewHole :: Backend.TypeEnv -> DropDownList.Model Model -> Reactive Model
viewHole typeEnv dropDownListModel =
    handleEvents <$>
      DropDownList.view
        (holeDropDownListSettings typeEnv)
        (calculateOptions typeEnv)
        dropDownListModel
  where
    handleEvents (Left dropDownListModel) = Hole dropDownListModel
    handleEvents (Right model) = model
