-- File created: 2009-07-15 10:37:42

module Haschoo.Utils where

import Data.List (intercalate)

swap :: (a,b) -> (b,a)
swap ~(a,b) = (b,a)

showScmList :: (a -> String) -> [a] -> String
showScmList f xs = concat ["(", intercalate " " (map f xs), ")"]

void :: Functor f => f a -> f ()
void = fmap (const ())

infixl 0 $<
($<) :: a -> (a -> b) -> b
($<) = flip ($)

infixl 8 .:
(.:) :: (d -> c) -> (a -> b -> d) -> a -> b -> c
f .: g = \x y -> f (g x y)
