{-# LANGUAGE ScopedTypeVariables #-}
module Widgets.DropDownList where

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

import qualified Widgets.Button as Button


data Model a
  = OpenDropDown Button.Model
  | DropDownList [Option a]
  deriving (Show, Eq)

data Option a
  = Option
  { optionButton :: Button.Model
  , optionResult :: a
  } deriving (Show, Eq)

data OptionWithEvent a
  = OptionWithEvent
  { optionEvent :: Button.Event
  , optionWithoutEvent :: Option a
  } deriving (Show, Eq)

construct :: Model a
construct = OpenDropDown Button.construct

view :: TextStyle -> String -> String -> (a -> Form) -> [a] -> Model a -> Reactive (Either (Model a) a)
view style buttonText _ renderModel models (OpenDropDown button) =
    Left <$> handleClick <$> Button.view (renderButton style buttonText) button
  where
    handleClick (buttonModel, buttonClicked)
      | buttonClicked = DropDownList (map (Option Button.construct) models)
      | otherwise     = OpenDropDown buttonModel
view style _ dropDownText renderModel _ (DropDownList options) =
    Reactive.onEvent
      (Event.mousePress
        (Event.buttonGuard MBLeft
          (Event.outsideGuard reactive handleOutsideClick)))
      reactive
  where
    handleOutsideClick :: Either (Model a) a -> Either (Model a) a
    handleOutsideClick (Left model) = Left (OpenDropDown Button.construct)
    handleOutsideClick (Right result) = Right result

    reactive =
      Reactive.attachFormTo up
        (text style dropDownText)
        buttonsViewed

    --buttonsViewed :: Reactive (Either (Model a) a)
    buttonsViewed =
      handleModelChange <$>
        Button.handleButtonList optionEvent <$>
          Reactive.besidesAll down (map viewButton options)

    handleModelChange (optionsWithEvent, Just clickedOption) = Right (optionResult (optionWithoutEvent clickedOption))
    handleModelChange (optionsWithEvent, Nothing) = Left (DropDownList (map optionWithoutEvent optionsWithEvent))

    viewButton option =
      makeOptionWithEvent option <$>
        (Button.view
          (padded 4 (renderModel (optionResult option)))
          (optionButton option))

    makeOptionWithEvent option (button, buttonEvent) =
      OptionWithEvent buttonEvent (option { optionButton = button })

renderButton :: TextStyle -> String -> Form
renderButton style = padded 4 . text style { textColor = darkGrey, fontSize = 10 }
