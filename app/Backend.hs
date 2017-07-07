module Backend where

import qualified Data.Map.Lazy as Map

data Type
  = Nat
  deriving (Show, Eq)

type TypeEnv = Map.Map String Type

stdTypeEnv :: TypeEnv
stdTypeEnv = Map.fromList
  [ ("Nat", Nat) ]
