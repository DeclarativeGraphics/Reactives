{-# LANGUAGE ScopedTypeVariables #-}
module Widgets.DropDownList where

import Graphics.Declarative.Transforms
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
import Control.Lens

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
  { optionIndex :: Int
  , optionEvent :: Button.Event
  , optionWithoutEvent :: Option a
  } deriving (Show, Eq)

data Settings a
  = Settings
  { textStyle :: TextStyle
  , buttonText :: String
  , dropDownText :: String
  , renderModel :: a -> Form
  , updateDropdown :: Model a -> a -- or 'embed model'? dunno
  }

construct :: Model a
construct = OpenDropDown Button.construct

view :: Settings a -> [a] -> Model a -> Reactive Input a
view settings models (OpenDropDown button) =
  let reactive = Button.view (renderButton settings) button
      handleClick (model, True) = Just (updateDropdown settings (DropDownList (map (Option False) models)))
      handleClick (model, False) = Just (updateDropdown settings (OpenDropDown model))
  in
    Reactive.processEvent handleClick reactive
view settings _ (DropDownList options) =
    Reactive.onEventOverriding
      (Event.mousePress
        (Event.buttonGuard MBLeft
          (Event.outsideGuard reactive (Just (updateDropdown settings (OpenDropDown False))))))
      (Reactive.processEvent handleOptionEvents reactive)
  where
    reactive =
      appendTo up
        [ Reactive.static (text (textStyle settings) (dropDownText settings))
        , buttonsViewed
        ]

    handleOptionEvents (OptionWithEvent index buttonEvent option) =
      if buttonEvent
        then Just (optionResult option)
        else Just (updateDropdown settings (DropDownList (set (element index) option options)))

    buttonsViewed =
      appendTo down (zipWith viewButton [0..] options)

    attachBulletPoint form =
      alignHV (0, 0)
        (appendTo left [ alignHV (0, 0.5) form, alignHV (0, 0.5) (text (textStyle settings) "•") ])

    viewButton index option =
      makeOptionWithEvent index option <$>
          (Button.view
            (attachBulletPoint (padded 4 (renderModel settings (optionResult option))))
            (optionButton option))

    makeOptionWithEvent index option (button, buttonEvent) =
      OptionWithEvent index buttonEvent (option { optionButton = button })

renderButton :: Settings a -> Form
renderButton settings =
    padded 4 (text ((textStyle settings) { textColor = darkGrey, fontSize = 10 }) (buttonText settings))
