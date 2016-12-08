module Event where

import Graphics.Declarative.SDL.Input
import Control.Monad (liftM2)

import Linear

mouseInput :: (MouseInput -> a -> a) -> (Input -> a -> a)
mouseInput changeModel (MouseInput input) = changeModel input
mouseInput _ _ = id

mousePress :: (MB -> V2 Double -> a -> a) -> (Input -> a -> a)
mousePress = mouseInput . mousePressGuard

mouseMove :: (V2 Double -> a -> a) -> (Input -> a -> a)
mouseMove = mouseInput . mouseMoveGuard

keyInput :: (KeyInput -> a -> a) -> (Input -> a -> a)
keyInput changeModel (KeyInput input) = changeModel input
keyInput _ _ = id

keyPress :: (Key -> a -> a) -> (Input -> a -> a)
keyPress = keyInput . keyPressGuard

textInput :: (String -> a -> a) -> (Input -> a -> a)
textInput changeModel (TextInput text) = changeModel text
textInput _ _ = id



mousePressGuard :: (MB -> V2 Double -> a -> a) -> (MouseInput -> a -> a)
mousePressGuard changeModel (MousePress pos button) = changeModel button pos
mousePressGuard _ _ = id

buttonGuard :: MB -> (V2 Double -> a -> a) -> (MB -> V2 Double -> a -> a)
buttonGuard shouldButton changeModel actualButton
  | shouldButton == actualButton = changeModel
  | otherwise                    = const id

mouseMoveGuard :: (V2 Double -> a -> a) -> (MouseInput -> a -> a)
mouseMoveGuard changeModel (MouseMove pos) = changeModel pos
mouseMoveGuard _ _ = id

keyPressGuard :: (Key -> a -> a) -> (KeyInput -> a -> a)
keyPressGuard changeModel (KeyPress key) = changeModel key
keyPressGuard _ _ = id

keyGuard :: Key -> (a -> a) -> (Key -> a -> a)
keyGuard shouldKey changeModel actualKey
  | shouldKey == actualKey = changeModel
  | otherwise              = id


handleAfter :: (Input -> b -> c) -> (Input -> a -> b) -> (Input -> a -> c)
handleAfter = liftM2 (.)

handleChain :: [(Input -> a -> a)] -> (Input -> a -> a)
handleChain = foldr handleAfter (const id)
