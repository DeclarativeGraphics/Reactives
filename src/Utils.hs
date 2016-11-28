module Utils where

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing x = x
