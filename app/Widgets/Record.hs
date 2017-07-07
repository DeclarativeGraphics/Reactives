module Widgets.Record where

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

data Model a
  = Record Button.Model [Association a]
  deriving (Show, Eq)

data Association a
  = Association
  { associationTextField :: TextField.Model
  , associationValue :: a
  , associationAddButton :: Button.Model
  , associationRemoveButton :: Button.Model
  , associationErrors :: [Error]
  } deriving (Show, Eq)

type Error = String

data Settings a
  = Settings
  { textStyle :: TextStyle
  , newInstance :: a
  , viewInner :: a -> Reactive a
  , placeholderText :: String
  , emptyRecord :: Form
  }

empty :: Model a
empty = Record Button.construct []

activeAssociation :: a -> Association a
activeAssociation inner =
  Association
    { associationTextField = TextField.emptyActive
    , associationValue = inner
    , associationAddButton = Button.construct
    , associationRemoveButton = Button.construct
    , associationErrors = []
    }

construct :: [(String, a)] -> Model a
construct = Record Button.construct . map makeModel
  where
    makeModel (str, model) = (activeAssociation model) { associationTextField = (TextField.inactive str) }



view :: Settings a -> Model a -> Reactive (Model a)
view settings (Record emptyButton []) =
    handleEvent <$> Button.view (emptyRecord settings) emptyButton
  where
    handleEvent (buttonModel, buttonClicked)
      | buttonClicked = Record buttonModel [activeAssociation (newInstance settings)]
      | otherwise     = Record buttonModel []
view settings (Record button associations) =
    Record button <$> Reactive.onVisual addBox (viewAssociations settings associations)
  where
    addBox = addBorder darkGrey . padded 4

viewAssociations :: Settings a -> [Association a] -> Reactive [Association a]
viewAssociations settings associations =
    concat <$> Reactive.besidesAll down (map (viewAssociation settings) associations)

viewAssociation :: Settings a -> Association a -> Reactive [Association a]
viewAssociation settings (Association textField innerValue buttonAdd buttonRemove errors) =
      (makeAssociation <$> alignHV (0, 0.5) nameReactive)
      `attachRight` (alignHV (0, 0.5) (viewInner settings innerValue))
      `attachRight` (alignHV (0, 0.5) buttonAddReactive)
      `attachRight` (alignHV (0, 0.5) buttonRemoveReactive)
      `attachDown` (viewErrors settings isAssociationActive errors)
  where
    isAssociationActive =
      or [TextField.isActive textField, Button.isActive buttonAdd, Button.isActive buttonRemove]

    makeAssociation textField innerValue (buttonAdd, buttonAddClicked) (buttonRemove, buttonRemoveClicked) errors
      | buttonRemoveClicked = []
      | buttonAddClicked = [assoc, activeAssociation (newInstance settings)]
      | otherwise = [assoc]
      where assoc = Association textField innerValue buttonAdd buttonRemove errors

    actionButton name buttonModel =
      alignHV (0, 0)
        (Reactive.attachFormTo left
          (gap 4 0)
          (Button.view (text (textStyle settings) name) buttonModel))

    buttonAddReactive =
      if isAssociationActive
        then actionButton " + " buttonAdd
        else Reactive.emptyR (buttonAdd, False)

    buttonRemoveReactive =
      if isAssociationActive
        then actionButton " X " buttonRemove
        else Reactive.emptyR (buttonRemove, False)

    attachRight = Reactive.attach right
    attachDown = Reactive.attach down

    possiblyAddBorder =
      if not (null errors) then addBorder red else id

    nameReactive =
      Reactive.attachFormTo right
        (text (textStyle settings) ": ")
        (Reactive.onVisual (possiblyAddBorder . padded 2)
          (TextField.view (textStyle settings) (placeholderText settings) textField))

viewErrors :: Settings a -> Bool -> [Error] -> Reactive [Error]
viewErrors settings _ [] = Reactive.emptyR []
viewErrors settings False errors = Reactive.emptyR errors
viewErrors settings True errors =
  Reactive.constant errors
    (appendTo down (map (padded 3 . addBackground red . padded 1 . text style) errors))
  where
    style = (textStyle settings) { textColor = lightGrey, fontSize = 10 }


inputValidation :: Model a -> Model a
inputValidation (Record button associations) = Record button (checkAssociations associations)

checkAssociations :: [Association a] -> [Association a]
checkAssociations assocs =
    map checkAssociation assocs
  where
    checkAssociation association =
        association
          { associationErrors = if nameAlreadyExists then ["Duplicate name"] else []
          }
      where
        associationName = assocName association
        nameAlreadyExists = length (filter (== associationName) existingNames) > 1

    assocName = TextField.getContent . associationTextField
    existingNames = map assocName assocs
