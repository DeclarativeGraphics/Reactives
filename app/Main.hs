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
import Utils (orElse)
import Linear

main :: IO ()
main = runReactive (move (V2 100 100) . viewExpr)
  (EAtom (TypeLit (textField "Form" "", False)) Hole)

dFont :: TextStyle
dFont = font "monospace" 18

data TextField = TextField String String

textField :: String -> String -> TextField
textField leftFromCaret rightFromCaret =
  TextField (reverse leftFromCaret) rightFromCaret

caretLeft :: TextField -> TextField
caretLeft (TextField (c:left) right) = TextField left (c:right)
caretLeft tf = tf

caretRight :: TextField -> TextField
caretRight (TextField left (c:right)) = TextField (c:left) right
caretRight tf = tf

deleteLeft :: TextField -> TextField
deleteLeft (TextField (c:left) right) = TextField left right
deleteLeft tf = tf

deleteRight :: TextField -> TextField
deleteRight (TextField left (c:right)) = TextField left right
deleteRight tf = tf

addText :: String -> TextField -> TextField
addText str (TextField left right) = TextField (reverse str ++ left) right

viewTextField :: TextStyle -> Bool -> TextField -> Reactive TextField
viewTextField textStyle drawCaret =
    Reactive.onEvent eventHandler
  . Reactive.fromModel renderTextField
  where
    eventHandler =
      Event.handleChain
        [ Event.keyPress (Event.keyGuard KeyLeft caretLeft)
        , Event.keyPress (Event.keyGuard KeyRight caretRight)
        , Event.keyPress (Event.keyGuard KeyBackspace deleteLeft)
        , Event.keyPress (Event.keyGuard KeyDelete deleteRight)
        , Event.textInput addText ]

    renderTextField (TextField leftFromCaret rightFromCaret) =
      appendTo right
        [ text textStyle (reverse leftFromCaret)
        , if drawCaret then collapseBorder (alignHV (0, 0) caret) else empty
        , text textStyle rightFromCaret ]

    caretHeight = graphicHeight (text textStyle "|")
    caret = filled black (rectangle (caretHeight*0.05) caretHeight)

type IsActive m = (m, Bool)

wrapActivation :: (Bool -> m -> Reactive m) -> IsActive m -> Reactive (IsActive m)
wrapActivation view (model, isActive) =
    Reactive.onEvent eventHandler innerReactive
  where
    innerReactive = fmap (flip (,) isActive) (view isActive model)

    handleMouseLeft pos (model, _) =
      (model, Reactive.isInside innerReactive pos)

    catchRest event (newModel, isActive)
      | isActive  = (newModel, isActive)
      | otherwise = (model, isActive)

    eventHandler =
      Event.handleChain
        [ Event.mousePress (Event.buttonGuard MBLeft handleMouseLeft)
        , catchRest ]

myTextField :: IsActive TextField -> Reactive (IsActive TextField)
myTextField = wrapActivation (viewTextField dFont)


data Expr
  = EAtom Type Atom

data Atom = Hole

data Type = TypeLit (IsActive TextField)


viewExpr :: Expr -> Reactive Expr
viewExpr (EAtom typ atom) =
    Reactive.besidesTo down EAtom
      typeWithLine
      (viewAtom atom)
  where
    typeWithLine =
      Reactive.attachFormTo down
        (padded 10 (filled black (rectangle 100 1)))
        (viewType typ)

viewType :: Type -> Reactive Type
viewType (TypeLit text) =
  Reactive.onVisual centeredHV $ TypeLit <$> myTextField text

viewAtom :: Atom -> Reactive Atom
viewAtom Hole =
  Reactive.constant Hole
    (outlined (solid grey) (rectangle 40 20))
