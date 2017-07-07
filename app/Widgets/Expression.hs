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
import Utils
import Linear
import FormUtils

import qualified Widgets.TextField as TextField
import qualified Widgets.Button as Button
import qualified Widgets.DropDownList as DropDownList
import qualified Widgets.Type as Type
import qualified Widgets.Record as Record

import qualified Backend

data Model
  = Hole --(DropDownList.Model Model)
  | App Model Model
  | Abs (Record.Model Type.Model) Model
  deriving (Show, Eq)

example :: Model
example = App (Abs (Record.construct [("", Type.hole)]) Hole) Hole

monoStyle :: TextStyle
monoStyle = defaultTextStyle { fontFamily = "monospace" }

recordSettings :: Record.Settings Type.Model
recordSettings = (Type.recordSettings Backend.stdTypeEnv)
  { Record.placeholderText = "parameter"
  , Record.emptyRecord = noParametersError }

noParametersError :: Form
noParametersError =
  padded 4 (addBackground red (text style "No Parameters"))
  where
    style = (Record.textStyle recordSettings) { textColor = lightGrey, fontSize = 10 }

view :: Model -> Reactive Model
view Hole = Reactive.constant Hole renderHole
view (App functionModel argumentModel) =
    alignHV (0, 0)
      (App
        <$> functionReactive
        `Reactive.attachRight` argumentReactive)
  where
    functionReactive =
      Reactive.attachFormTo right (padded 4 applicationArrow)
        (alignHV (0, 0.5) (view functionModel))
    argumentReactive = alignHV (0, 0.5) (view argumentModel)
view (Abs parametersRecord bodyModel) =
    Abs
      <$> parametersReactive
      `Reactive.attachDown` bodyReactive
  where
    parametersReactive =
      Reactive.attachFormTo down (gap 0 4)
        (Reactive.attachFormTo left
          (appendTo right [ alignHV (0, 1) (text monoStyle "λ"), gap 8 0 ])
          (alignHV (0, 1) (Record.view recordSettings parametersRecord)))
    bodyReactive =
      Reactive.attachFormTo left
        (appendTo right [ text monoStyle "→", gap 6 0 ])
        (alignHV (0, 0) (view bodyModel))

renderHole :: Form
renderHole = addBorder grey (padded 2 (text monoStyle { textColor = grey } "?value?"))

applicationArrow :: Form
applicationArrow =
  outlined (solid darkGrey) (bordered (0, 0.5) 6 8 applicationArrowShape)

applicationArrowShape :: Shape
applicationArrowShape =
  closedPath     (pathPoint (0, -4)
    `lineConnect` pathPoint (6, 0)
    `lineConnect` pathPoint (0, 4))
