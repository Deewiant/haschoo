-- File created: 2009-07-15 10:37:42

module Haschoo.Utils where

import Control.Applicative   ((<$>))
import Control.Arrow         (first)
import Control.Monad         (liftM)
import Control.Monad.Error   () -- Monad ErrOr
import Control.Monad.State   (MonadState, get, put)
import Control.Monad.Trans   (MonadIO, liftIO)
import Data.List             (intercalate)
import System.IO.Unsafe      (unsafeInterleaveIO)
import System.Mem.StableName (makeStableName)

type ErrOr = Either String

swap :: (a,b) -> (b,a)
swap ~(a,b) = (b,a)

-- Returns the length of the shorter of the two
compareLengths :: [a] -> [b] -> (Ordering, Int)
compareLengths = go 0
 where
   go n []     []     = (EQ, n)
   go n (_:_)  []     = (GT, n)
   go n []     (_:_)  = (LT, n)
   go n (_:as) (_:bs) = go (n+1) as bs

compareLength :: [a] -> Int -> Ordering
compareLength _      n | n < 0 = GT
compareLength []     0         = EQ
compareLength []     _         = LT
compareLength (_:_)  0         = GT
compareLength (_:as) n         = compareLength as (n-1)

initLast :: [a] -> ([a], a)
initLast [x]    = ([], x)
initLast (x:xs) = first (x:) (initLast xs)
initLast []     = error "initLast :: empty list"

initLast2Maybe :: [a] -> Maybe ([a], a, a)
initLast2Maybe [x,y]  = Just ([], x, y)
initLast2Maybe (a:as) = fmap (\ ~(bs,x,y) -> (a:bs,x,y)) (initLast2Maybe as)
initLast2Maybe []     = Nothing

-- eqWithM p xs ys is kind of like allM (uncurry p) (zip xs ys) but takes into
-- account the lengths of the lists (differing lengths aren't equal)
--
-- Essentially, monadic list equality according to the given predicate
eqWithM :: Monad m => (a -> b -> m Bool) -> [a] -> [b] -> m Bool
eqWithM _ [] [] = return True
eqWithM _ _  [] = return False
eqWithM _ [] _  = return False
eqWithM p (x:xs) (y:ys) = do
   b <- p x y
   if b
      then eqWithM p xs ys
      else return False

showScmList :: (a -> IO String) -> [a] -> IO String
showScmList f xs =
   ("("++) . (++")") . intercalate " " <$> lazyMapM f xs

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

infixr 9 .:
(.:) :: (d -> c) -> (a -> b -> d) -> a -> b -> c
f .: g = \x y -> f (g x y)

ptrEq :: a -> a -> IO Bool
ptrEq x y = do
   nx <- makeStableName x
   ny <- makeStableName y
   return (nx == ny)
