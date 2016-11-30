{-# LANGUAGE ExistentialQuantification #-}
module RunReactive where

import qualified SDL

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import Graphics.Declarative.SDL.Input (waitEventTimeout, ticks)
import Graphics.Declarative.SDL.Glue

import qualified Reactive
import Reactive (Reactive(..), atopReactives)
import Utils (orElse)

runReactive :: Show model => (model -> Reactive model) -> model -> IO ()
runReactive view initial = runSDL $
  withWindow "DeclarativeGraphics-SDL" SDL.defaultWindow $ \window ->
  withRenderer SDL.defaultRenderer window $ \renderer ->
  runReactiveInSDL view initial window renderer

runReactiveInSDL :: Show model => (model -> Reactive model) -> model -> SDL.Window -> SDL.Renderer -> IO ()
runReactiveInSDL view initial window renderer = loop (initial, view initial)
  where
    loop state = do
      time <- ticks
      renderCairoViaTexture
        (cairoClear >> drawForm (visual $ snd state))
        window
        renderer
      (newModel, newReactive) <- workUntil (time + 16) state
      loop (newModel, newReactive)

    step event (prevModel, prevReactive) =
      let newModel = Reactive.react prevReactive event
          newReactive = view newModel
       in return (newModel, newReactive)

    workUntil stopTime state = do
      time <- ticks
      let timeLeft = fromIntegral stopTime - fromIntegral time :: Int
      if timeLeft <= 0
        then return state
        else do
          maybeInput <- waitEventTimeout (fromIntegral timeLeft `div` 2)
          case maybeInput of
            Just input -> do
              newState <- step input state
              workUntil stopTime newState
            Nothing    -> do
              workUntil stopTime state


optionalUpdate :: (m -> Reactive (Maybe m)) -> m -> Reactive m
optionalUpdate view model = fmap (`orElse` model) (view model)
