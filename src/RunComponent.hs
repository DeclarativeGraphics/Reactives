module RunComponent where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys

import qualified Reactive
import Reactive (Reactive(..))
import qualified Event
import RunReactive (runReactive)
import Data.Monoid (First(..))
import Utils (orElse, isInside, orTry, rightAngle)
import Linear

import Component

runComponent :: Component a -> IO ()
runComponent = runReactive componentToReactive

componentToReactive :: Component a -> Reactive (Component a)
componentToReactive component =
  Reactive
  { react = dispatch component
  , visual = render component
  }
