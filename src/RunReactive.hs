{-# LANGUAGE ExistentialQuantification #-}
module RunReactive where

import qualified SDL

import Graphics.Declarative.Physical2D
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

runReactive :: (model -> Reactive model) -> model -> IO ()
runReactive view initial = runSDL $
  withWindow "DeclarativeGraphics-SDL" SDL.defaultWindow $ \window ->
  withRenderer SDL.defaultRenderer window $ \renderer ->
  runReactiveInSDL view initial window renderer

runReactiveInSDL :: (model -> Reactive model) -> model -> SDL.Window -> SDL.Renderer -> IO ()
runReactiveInSDL view initial window renderer = loop (initial, view initial)
  where
    loop state = do
      time <- ticks
      (newModel, newReactive) <- workUntil (time + 16) state
      renderCairoViaTexture
        (cairoClear >> drawForm (visual newReactive))
        window
        renderer
      loop (newModel, newReactive)

    step event (prevModel, prevReactive) =
      let newModel = Reactive.react prevReactive event
          newReactive = view newModel
       in return (newModel, newReactive)

    workUntil stopTime state = do
      time <- ticks
      if time >= stopTime
        then return state
        else do
          maybeInput <- waitEventTimeout ((stopTime - time) `div` 2)
          case maybeInput of
            Just input -> do
              newState <- step input state
              workUntil stopTime newState
            Nothing    ->
              workUntil stopTime state


optionalUpdate :: (m -> Reactive (Maybe m)) -> m -> Reactive m
optionalUpdate view model = fmap (`orElse` model) (view model)




{-
data RProgram model
  = forall message . RProgram
  { update :: message -> model -> model
  , view :: model -> Reactive message
  , initial :: model }

runReactiveProgram :: RProgram model -> IO ()
runReactiveProgram (RProgram update view state) =
    runFormProgram (0.5, 0.5) (state, view state) step
  where
    step event (prevState, prevReactive) =
      let message = Reactive.react prevReactive event
          newState = update message prevState
          newReactive = view newState
       in return ((newState, newReactive), Reactive.visual newReactive)

updateOnJust :: (message -> model -> model) -> Maybe message -> model -> model
updateOnJust update (Just message) model = update message model
updateOnJust update Nothing model = model

data Or a b = Fst a | Snd b

combinePrograms :: RProgram a -> RProgram b -> RProgram (a, b)
combinePrograms
  (RProgram updateA viewA initialA)
  (RProgram updateB viewB initialB) =
    RProgram mUpdate mView (initialA, initialB)
  where
    mUpdate (msgA, msgB) (a, b) =
        ( updateA msgA a
        , updateB msgB b )
    mView (a, b) = atopReactives (viewA a) (viewB b)

{-
data ListZipper a
  = ListZipper [a] a [a]
  deriving (Show, Eq)

zippers :: [a] -> [ListZipper a]
zippers = zippersWalker []

zippersWalker :: [a] -> [a] -> [ListZipper a]
zippersWalker [] [] = []
zippersWalker left [a] = [ListZipper left a []]
zippersWalker left (x:xs) = ListZipper left x xs : zippersWalker (x:left) xs

zipperElem :: ListZipper a -> a
zipperElem (ListZipper _ a _) = a

zipperToList :: ListZipper a -> [a]
zipperToList (ListZipper left a right) = reverse left ++ a ++ right

listOf :: [RProgram model] -> RProgram [model]
listOf rprograms = RProgram mUpdate mView (map initial rprograms)
  where
    mUpdate (zipper, msg) ls =
-}
-}
