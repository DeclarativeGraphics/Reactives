module DomLike.Event where

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
import Utils (isInside)
import Linear


data Event
    = GainFocus
    | LoseFocus
    | MouseEnter
    | MouseLeave
    | Input Input

instance Transformable Event where
    transformBy matrix (Input i) = Input (transformBy matrix i)
    transformBy _ anythingElse = anythingElse

atopDomlike :: Reactive Event (Maybe a) -> Reactive Event (Maybe a) -> Reactive Event (Maybe a)
atopDomlike reactiveA reactiveB = _
