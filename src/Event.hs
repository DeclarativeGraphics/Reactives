module Event where

import Graphics.Declarative.SDL.Input
import Graphics.Declarative.Bordered
import Control.Monad (liftM2)
import Data.Monoid

import Linear
import Utils (isInside)

mouseInput :: (MouseInput -> Maybe a) -> (Input -> Maybe a)
mouseInput changeModel (MouseInput input) = changeModel input
mouseInput _ _ = Nothing

mousePress :: (MB -> V2 Double -> Maybe a) -> (Input -> Maybe a)
mousePress = mouseInput . mousePressGuard

mouseRelease :: (MB -> V2 Double -> Maybe a) -> (Input -> Maybe a)
mouseRelease = mouseInput . mouseReleaseGuard

mouseMove :: (V2 Double -> Maybe a) -> (Input -> Maybe a)
mouseMove = mouseInput . mouseMoveGuard

keyInput :: (KeyInput -> Maybe a) -> (Input -> Maybe a)
keyInput changeModel (KeyInput input) = changeModel input
keyInput _ _ = Nothing

keyPress :: (Key -> Maybe a) -> (Input -> Maybe a)
keyPress = keyInput . keyPressGuard

textInput :: (String -> Maybe a) -> (Input -> Maybe a)
textInput changeModel (TextInput text) = changeModel text
textInput _ _ = Nothing



mousePressGuard :: (MB -> V2 Double -> Maybe a) -> (MouseInput -> Maybe a)
mousePressGuard changeModel (MousePress pos button) = changeModel button pos
mousePressGuard _ _ = Nothing

mouseReleaseGuard :: (MB -> V2 Double -> Maybe a) -> (MouseInput -> Maybe a)
mouseReleaseGuard changeModel (MouseRelease pos button) = changeModel button pos
mouseReleaseGuard _ _ = Nothing

buttonGuard :: MB -> (V2 Double -> Maybe a) -> (MB -> V2 Double -> Maybe a)
buttonGuard shouldButton changeModel actualButton
  | shouldButton == actualButton = changeModel
  | otherwise                    = const Nothing

insideGuard :: HasBorder b => b -> Maybe a -> V2 Double -> Maybe a
insideGuard bordered maybeMsg pos
  | isInside bordered pos = maybeMsg
  | otherwise             = Nothing

outsideGuard :: HasBorder b => b -> Maybe a -> V2 Double -> Maybe a
outsideGuard bordered maybeMsg pos
  | isInside bordered pos = Nothing
  | otherwise             = maybeMsg


mouseMoveGuard :: (V2 Double -> Maybe a) -> (MouseInput -> Maybe a)
mouseMoveGuard changeModel (MouseMove pos) = changeModel pos
mouseMoveGuard _ _ = Nothing

keyPressGuard :: (Key -> Maybe a) -> (KeyInput -> Maybe a)
keyPressGuard changeModel (KeyPress key) = changeModel key
keyPressGuard _ _ = Nothing

keyGuard :: Key -> (Maybe a) -> (Key -> Maybe a)
keyGuard shouldKey changeModel actualKey
  | shouldKey == actualKey = changeModel
  | otherwise              = Nothing


handleAfter :: (Input -> b -> c) -> (Input -> a -> b) -> (Input -> a -> c)
handleAfter = liftM2 (.)

handleChain :: [Input -> Maybe a] -> (Input -> Maybe a)
handleChain handlers input = getFirst (mconcat (map First (sequence handlers input)))
