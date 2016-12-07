module Utils where

import Data.Monoid (First(..), getFirst)

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing x = x

orTry :: Maybe a -> Maybe a -> Maybe a
orTry a b = getFirst $ mappend (First a) (First b)
