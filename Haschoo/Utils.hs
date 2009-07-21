-- File created: 2009-07-15 10:37:42

module Haschoo.Utils where

import Control.Monad       (liftM)
import Control.Monad.Error () -- Monad ErrOr
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List           (intercalate)
import System.IO.Unsafe    (unsafeInterleaveIO)

type ErrOr = Either String

swap :: (a,b) -> (b,a)
swap ~(a,b) = (b,a)

compareLengths :: [a] -> [b] -> Ordering
compareLengths []     []     = EQ
compareLengths (_:_)  []     = GT
compareLengths []     (_:_)  = LT
compareLengths (_:as) (_:bs) = compareLengths as bs

compareLength :: [a] -> Int -> Ordering
compareLength _      n | n < 0 = GT
compareLength []     0         = EQ
compareLength []     _         = LT
compareLength (_:_)  0         = GT
compareLength (_:as) n         = compareLength as (n-1)

showScmList :: (a -> String) -> [a] -> String
showScmList f xs = concat ["(", intercalate " " (map f xs), ")"]

void :: Functor f => f a -> f ()
void = fmap (const ())

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f = liftM and . mapM f

lazyMapM :: MonadIO io => (a -> IO b) -> [a] -> io [b]
lazyMapM f = mapM (liftIO . unsafeInterleaveIO . f)

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM = (put =<<) . (get >>=)

infixl 0 $<
($<) :: a -> (a -> b) -> b
($<) = flip ($)

infixl 8 .:
(.:) :: (d -> c) -> (a -> b -> d) -> a -> b -> c
f .: g = \x y -> f (g x y)
