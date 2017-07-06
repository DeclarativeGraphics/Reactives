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

import qualified Widgets.TextField as TextField
import qualified Widgets.Button as Button

data Model -- for types
  = Record [Association]
  | Var TextField.Model
  deriving (Show, Eq)

data Association
  = Association
  { associationTextField :: TextField.Model
  , associationValue :: Model
  , associationAddButton :: Button.Model
  , associationErrors :: [Error]
  } deriving (Show, Eq)

type Error = String

monoStyle :: TextStyle
monoStyle = defaultTextStyle { fontFamily = "monospace" }

example :: Model
example
  = record
  [ ( "key", var "value" )
  , ( "record"
    , record
      [ ( "nested", var "records" )
      , ( "mutliple", var "fields" )
      , ( "all good are", var "3" )
      ]
    )
  ]

record :: [(String, Model)] -> Model
record = Record . map makeModel
  where makeModel (str, model) = Association (TextField.inactive str) model Button.construct []

var :: String -> Model
var = Var . TextField.inactive

newAssociation :: Association
newAssociation =
  Association
    { associationTextField = TextField.emptyActive
    , associationValue = (Var TextField.emptyInactive)
    , associationAddButton = Button.construct
    , associationErrors = []
    }


view :: Model -> Reactive Model
view (Var textField) = Var <$> TextField.view monoStyle "variable" textField
view (Record associations) = Record <$> viewAssociations associations

viewAssociations :: [Association] -> Reactive [Association]
viewAssociations associations =
    concat <$> Reactive.besidesAll down (map viewAssociation associations)

viewAssociation :: Association -> Reactive [Association]
viewAssociation (Association textField typ button errors) =
      (makeAssociation <$> nameReactive)
      `attachRight` (view typ)
      `attachRight` buttonReactive
      `attachDown` (viewErrors isAssociationActive errors)
  where
    isAssociationActive =
      TextField.isActive textField || Button.isActive button

    makeAssociation textField typ (button, buttonClicked) errors =
      possiblyAddAssociation buttonClicked
        (Association textField typ button errors)

    possiblyAddAssociation buttonClicked assoc
      | buttonClicked = [assoc, newAssociation]
      | otherwise = [assoc]

    addButton =
      alignHV (0, 0)
        (Reactive.attachFormTo left
          (gap 4 0)
          (Button.view (text monoStyle " + ") button))

    buttonReactive =
      if isAssociationActive
        then addButton
        else Reactive.emptyR (button, False)

    attachRight = Reactive.attach right
    attachDown = Reactive.attach down

    possiblyAddBorder =
      if not (null errors) then addBorder red else id

    nameReactive =
      Reactive.attachFormTo right
        (text monoStyle ": ")
        (Reactive.onVisual (possiblyAddBorder . padded 2)
          (TextField.view monoStyle "field name" textField))

viewErrors :: Bool -> [Error] -> Reactive [Error]
viewErrors _ [] = Reactive.emptyR []
viewErrors False errors = Reactive.emptyR errors
viewErrors True errors =
  Reactive.constant errors
    (appendTo down (map (padded 3 . addBackground red . padded 1 . text style) errors))
  where
    style = defaultTextStyle { textColor = lightGrey, fontFamily = "monospace", fontSize = 10 }

inputValidation :: Model -> Model
inputValidation (Var textField) = Var textField
inputValidation (Record associations) =
    Record (checkAssociations associations)

checkAssociations :: [Association] -> [Association]
checkAssociations assocs =
    map checkAssociation assocs
  where
    checkAssociation association =
        association
          { associationValue = inputValidation (associationValue association)
          , associationErrors = if nameAlreadyExists then ["Duplicate name"] else []
          }
      where
        associationName = TextField.getContent (associationTextField association)
        nameAlreadyExists = length (filter (== associationName) existingNames) > 1

    existingNames = map (TextField.getContent . associationTextField) assocs
