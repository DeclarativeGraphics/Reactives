module Components.Record where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys
import Linear
import Utils (orElse)
import FormUtils

import Component
import qualified Event

import qualified Components.TextField as TextField
import qualified Components.List as List
import qualified Components.Beside as Beside

{-
So now I want to add input validation. If two record fields have the same name,
i want to add a visual error.

I'd change the data Type:
data Type
  = Record [(String, Type, [Error])]
  | Var String

type Error = String

And i want to add a function that does the checking and annotating:

inputValidation :: Type -> Type
inputValidation (Var str) = Var str
inputvalidation (Record associations) =
  checkDupAnnotate (map (onSnd inputvalidation) associations)

but

when I do I run this?
Lets assume I have a component of a time step "component :: Component Type"
I compute the next component by: "make (inputValidation (value component))".
Now I have a new component. But all focus information is lost. This is not acceptable.
The solution would be to expose all focus information into the component. But now
I lose the "power" of hiding that information, which makes this nice "Type" type possible.
And when I expost all focus information into the model, I can simply go back to Reactives.
That's what I'm going to do now. No need for "dispatch :: Input -> Component a".
I'll go "react :: Input -> model".
-}


make :: Type -> Component Type
make (Var name) = Var <$> TextField.makeHidden name
make (Record (association:associations)) =
    Record <$> List.make down (makeAssoc association) (makeAssoc <$> associations)

makeAssoc :: (String, Type) -> Component (String, Type)
makeAssoc (name, typ) =
    Beside.makeHidden right nameTextField (make typ)
  where
    nameTextField =
      attachFormTo right
        (text defaultTextStyle ": ")
        (TextField.makeHidden name)

data Type
  = Record [(String, Type)]
  | Var String
  deriving (Show, Eq)

exampleType :: Type
exampleType
  = Record
  [ ("eks de", Var "lol")
  , ("a record"
    , Record
    [ ("test", Var "this")
    , ("out", Var "look")
    ] )
  ]
