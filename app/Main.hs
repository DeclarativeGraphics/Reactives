module Main where

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
import Utils (orElse, isInside, orTry, rightAngle)
import Linear
import FormUtils

import qualified Widgets.Expression as Expression
import Widgets.Expression (ExprModel(..), TypeModel(..))
import qualified Widgets.Record as Record
import Widgets.Record (RecordModelFocused(..), RecordFieldModelFocused(..))
import Widgets.Activatable (ActiveOr(..))
import qualified Widgets.TextField as TextField
import Widgets.RecordType as RT

main :: IO ()
main = runReactive (move (V2 100 100) . alignHV (0, 0) . RT.view)
  (RT.TypeModel RT.NumberType
    (RT.InRecordType
      (RT.InRecordType RT.Top [] "pos" [("name", RT.TypeHole)])
      [("x", RT.NumberType)] "y" []))
{-main = runReactive (move (V2 100 100) . alignHV (0, 0) . Record.viewFocused)
  (RecordFocused [] (RecordFieldFocused (TextField.construct "Field1" "")) [])
main = runReactive (move (V2 100 100) . alignHV (0, 0) . Expression.view)
  (ValueHole (TypeHole Nothing) Nothing)
-}

example =
  (RT.TypeModel RT.NumberType
    (RT.InRecordType
      (RT.InRecordType RT.Top [] "pos" [("name", RT.TypeHole)])
      [("x", RT.NumberType)] "y" []))
