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

data Settings a
  = Settings
  { textStyle :: TextStyle
  , buttonText :: String
  , dropDownText :: String
  , renderModel :: a -> Form
  }

construct :: Model a
construct = OpenDropDown Button.construct

view :: Settings a -> [a] -> Model a -> Reactive Input (Either (Model a) a)
view settings models (OpenDropDown button) =
    Left <$> handleClick <$> Button.view (renderButton settings) button
  where
    handleClick (buttonModel, buttonClicked)
      | buttonClicked = DropDownList (map (Option Button.construct) models)
      | otherwise     = OpenDropDown buttonModel
view settings _ (DropDownList options) =
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
        (text (textStyle settings) (dropDownText settings))
        buttonsViewed

    buttonsViewed =
      handleModelChange <$>
        Button.handleButtonList optionEvent <$>
          Reactive.besidesAll down (map viewButton options)

    handleModelChange (optionsWithEvent, Just clickedOption) = Right (optionResult (optionWithoutEvent clickedOption))
    handleModelChange (optionsWithEvent, Nothing) = Left (DropDownList (map optionWithoutEvent optionsWithEvent))

    attachBulletPoint form =
      alignHV (0, 0)
        (appendTo left [ alignHV (0, 0.5) form, alignHV (0, 0.5) (text (textStyle settings) "•") ])

    viewButton option =
      makeOptionWithEvent option <$>
          (Button.view
            (attachBulletPoint (padded 4 (renderModel settings (optionResult option))))
            (optionButton option))

    makeOptionWithEvent option (button, buttonEvent) =
      OptionWithEvent buttonEvent (option { optionButton = button })

renderButton :: Settings a -> Form
renderButton settings =
    padded 4 (text ((textStyle settings) { textColor = darkGrey, fontSize = 10 }) (buttonText settings))
