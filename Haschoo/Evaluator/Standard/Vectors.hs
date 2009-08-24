-- File created: 2009-08-06 20:47:49

module Haschoo.Evaluator.Standard.Vectors (procedures) where

import Control.Applicative ((<$>))
import Data.Array          (Array)
import Data.Array.IArray   ((!), bounds, elems)
import Data.Array.IO       (IOArray)
import Data.Array.MArray   ( readArray, writeArray
                           , newArray, newListArray
                           , getBounds, getElems)

import Haschoo.Types           (ScmValue(..), listToPair, pairToList)
import Haschoo.Utils           (ErrOr)
import Haschoo.Evaluator.Utils ( tooFewArgs, tooManyArgs, immutable
                               , notInt, notList)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b))
   [ ("vector?", return . fmap ScmBool . scmIsVector)

   , ("make-vector", scmMakeVector)
   , ("vector",      scmVector)

   , ("vector-length", scmLength)

   , ("vector-ref",  scmRef)
   , ("vector-set!", scmSet)

   , ("vector->list", scmToList)
   , ("list->vector", scmFromList)

   , ("vector-fill!", scmFill)
   ]

scmIsVector :: [ScmValue] -> ErrOr Bool
scmIsVector [ScmMVector _] = Right True
scmIsVector [ScmVector  _] = Right True
scmIsVector [_]            = Right False
scmIsVector []             = tooFewArgs  "vector?"
scmIsVector _              = tooManyArgs "vector?"

scmMakeVector, scmVector :: [ScmValue] -> IO (ErrOr ScmValue)
scmMakeVector (ScmInt l : xs) =
   case xs of
        []  -> f (flip newArray ScmVoid)
        [x] -> f (flip newArray x)
        _   -> return$ tooManyArgs "make-vector"
 where
   f mk = tryToLen "make-vector" l $ \i -> ScmMVector <$> mk (0, i-1)

scmMakeVector (_:_:_:_) = return$ tooManyArgs "make-vector"
scmMakeVector []        = return$ tooFewArgs  "make-vector"
scmMakeVector _         = return$ notInt      "make-vector"

scmVector xs = Right <$> toScmMVector xs

scmLength :: [ScmValue] -> IO (ErrOr ScmValue)
scmLength [x] = case x of
                     ScmVector  s -> Right . ScmInt . toInteger <$>  vLen s
                     ScmMVector s -> Right . ScmInt . toInteger <$> mvLen s
                     _            -> return$ notVector "vector-length"

scmLength [] = return$ tooFewArgs  "vector-length"
scmLength _  = return$ tooManyArgs "vector-length"

scmRef, scmSet :: [ScmValue] -> IO (ErrOr ScmValue)
scmRef [x, ScmInt i] = case x of
                            ScmVector  s -> f (vLen  s) (return . (s!))
                            ScmMVector s -> f (mvLen s) (readArray s)
                            _            -> return$ notVector "vector-ref"
 where
   f len = tryToIdx "vector-ref" len i

scmRef [_,_]   = return$ notInt      "vector-ref"
scmRef (_:_:_) = return$ tooManyArgs "vector-ref"
scmRef _       = return$ tooFewArgs  "vector-ref"

scmSet [v, ScmInt idx, x] =
   case v of
        ScmMVector v' -> do
           tryToIdx "vector-set!" (mvLen v') idx (\i -> writeArray v' i x)
           return (Right ScmVoid)

        ScmVector  _ -> return$ immutable "vector-set!"
        _            -> return$ notVector "vector-set!"

scmSet [_, _, ScmChar _] = return$ notInt      "vector-set!"
scmSet (_:_:_:_)         = return$ tooManyArgs "vector-set!"
scmSet _                 = return$ tooFewArgs  "vector-set!"

scmToList, scmFromList :: [ScmValue] -> IO (ErrOr ScmValue)
scmToList [x] = case x of
                     ScmVector  v -> f (elems v)
                     ScmMVector v -> f =<< getElems v
                     _            -> return$ notVector "vector->list"
 where
   f = fmap (Right . fst) . listToPair

scmToList [] = return$ tooFewArgs  "vector->list"
scmToList _  = return$ tooManyArgs "vector->list"

scmFromList [x] =
   case x of
        ScmList l       -> Right <$> toScmMVector l
        p@(ScmPair _ _) -> do
           l <- pairToList p
           case l of
                Left _             -> return$ notList "list->vector"
                Right (ScmList l') -> Right <$> toScmMVector l'

                _ -> error "list->vector :: the impossible happened"

        _ -> return$ notList "list->vector"

scmFromList [] = return$ tooFewArgs  "list->vector"
scmFromList _  = return$ tooManyArgs "list->vector"

scmFill :: [ScmValue] -> IO (ErrOr ScmValue)
scmFill [v, x] =
   case v of
        ScmMVector v' -> do
           (a,b) <- getBounds v'
           mapM_ (\i -> writeArray v' i x) [a..b]
           return (Right ScmVoid)

        ScmVector _ -> return$ immutable "vector-fill!"
        _           -> return$ notVector "vector-fill!"

scmFill (_:_:_) = return$ tooManyArgs "vector-fill!"
scmFill _       = return$ tooFewArgs  "vector-fill!"

------

notVector :: String -> ErrOr a
notVector = fail . ("Nonvector argument to " ++)

toScmMVector :: [ScmValue] -> IO ScmValue
toScmMVector v = ScmMVector <$> newListArray (0, length v - 1) v

tryToLen :: String -> Integer -> (Int -> IO a) -> IO (ErrOr a)
tryToLen = tryRange 0 (toInteger (maxBound :: Int))

tryToIdx :: String -> IO Int -> Integer -> (Int -> IO a) -> IO (ErrOr a)
tryToIdx s mlen i f = mlen >>= \len -> tryRange 0 (toInteger len - 1) s i f

tryRange :: Integer -> Integer -> String -> Integer -> (Int -> IO a)
         -> IO (ErrOr a)
tryRange a b s i f = if i >= a && i <= b
                        then Right <$> f (fromInteger i)
                        else return.Left $ "Out-of-range integer to " ++ s

vLen :: Array Int ScmValue -> IO Int
vLen = return . (+1) . snd . bounds

mvLen :: IOArray Int ScmValue -> IO Int
mvLen = fmap ((+1) . snd) . getBounds
