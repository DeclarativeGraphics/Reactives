module Main where

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
import Utils (orElse, isInside, orTry, rightAngle)
import Linear
import FormUtils

import qualified Widgets.TextField as TextField
import qualified Widgets.Type as Type
import qualified Widgets.Expression as Expression
import qualified Widgets.Record as Record

import qualified Backend

main :: IO ()
main =
  runReactive (move (V2 200 200) . Expression.view) Expression.example
  --runReactive (move (V2 200 200) . Type.view Backend.stdTypeEnv) Type.example
