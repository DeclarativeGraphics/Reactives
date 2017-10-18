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
  | TypeReference String
  | Hole (DropDownList.Model Model)
  | Arrow Model Model
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
  [ ( "key", ref "value" )
  , ( "record"
    , record
      [ ( "nested", ref "records" )
      , ( "mutliple", ref "fields" )
      , ( "all good are", ref "3" )
      , ( "holes are this", hole )
      ]
    )
  ]

record :: [(String, Model)] -> Model
record = RecordType . Record.construct

ref :: String -> Model
ref = TypeReference

hole :: Model
hole = Hole DropDownList.construct


typeToModel :: Backend.Type -> Model
typeToModel Backend.Nat = ref "Nat"
typeToModel (Backend.RecordType r) =
    record (Map.assocs (Map.map typeToModel r))


calculateOptions :: Backend.TypeEnv -> [Model]
calculateOptions typeEnv =
  [ Arrow hole hole
  ] ++ map typeToModel (Map.elems typeEnv)


view :: Backend.TypeEnv -> Model -> Reactive Model
view typeEnv (TypeReference name) = Reactive.constant (TypeReference name) (text monoStyle name)
view typeEnv (RecordType record) = RecordType <$> Record.view (recordSettings typeEnv) record
view typeEnv (Hole dropDownList) = viewHole typeEnv dropDownList
view typeEnv (Arrow argumentType resultType) =
    alignHV (0, 0)
      (Arrow
        <$> attachArrow (alignHV (0, 0.5) (view typeEnv argumentType))
        `Reactive.attachRight` (alignHV (0, 0.5) (view typeEnv resultType)))
  where
    attachArrow =
      Reactive.attachFormTo right
        (alignHV (0, 0.5)
          (text monoStyle { textColor = blue } " → "))



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
