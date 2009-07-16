-- File created: 2009-07-15 21:15:44

{-# LANGUAGE FlexibleInstances, Rank2Types, TypeSynonymInstances #-}

module Haschoo.Evaluator.Primitives.Numeric (primitives) where

import Control.Applicative ((<$>))
import Control.Arrow       ((&&&))
import Control.Monad       (ap, foldM)
import Data.Complex        (Complex((:+)), imagPart, realPart)
import Data.Function       (on)

import Haschoo.ScmValue (ScmValue(..))
import Haschoo.Utils    (ErrOr, allM, ($<), (.:))

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("number?",   fmap ScmBool . scmIsNumber)
   , ("complex?",  fmap ScmBool . scmIsNumber)
   , ("real?",     fmap ScmBool . scmIsReal)
   , ("rational?", fmap ScmBool . scmIsRational)
   , ("integer?",  fmap ScmBool . scmIsInteger)

   , "exact?"   $< id &&& fmap  ScmBool        .: scmIsExact
   , "inexact?" $< id &&& fmap (ScmBool . not) .: scmIsExact

   , "="  $< id &&& fmap ScmBool .: scmCompare (==)
   , "<"  $< id &&& fmap ScmBool .: scmCompare (<)
   , ">"  $< id &&& fmap ScmBool .: scmCompare (>)
   , "<=" $< id &&& fmap ScmBool .: scmCompare (<=)
   , ">=" $< id &&& fmap ScmBool .: scmCompare (>=)

   , ("zero?",     fmap ScmBool . scmIsZero)
   , ("positive?", fmap ScmBool . scmIsPos)
   , ("negative?", fmap ScmBool . scmIsNeg)

   , ("+", scmPlus)
   , ("-", scmMinus)
   , ("*", scmMul)
   , ("/", scmDiv)
   ]

---- Predicates

scmIsNumber, scmIsReal, scmIsRational, scmIsInteger :: [ScmValue] -> ErrOr Bool
scmIsNumber [x] = Right $ isNumeric x
scmIsNumber _   = tooManyArgs "number?"

scmIsReal [ScmComplex (_ :+ b)] = Right $ b == 0
scmIsReal [x]                   = Right $ isNumeric x
scmIsReal _                     = tooManyArgs "real?"

scmIsRational [ScmRat _] = Right True
scmIsRational [_]        = Right False
scmIsRational _          = tooManyArgs "rational?"

scmIsInteger [ScmInt     _]      = Right True
scmIsInteger [ScmRat     x]      = Right $ x == fromInteger (round x)
scmIsInteger [ScmReal    x]      = Right $ x == fromInteger (round x)
scmIsInteger [ScmComplex (a:+b)] = Right $ b == 0 && a == fromInteger (round a)
scmIsInteger [_]                 = Right False
scmIsInteger _                   = tooManyArgs "integer?"

scmIsExact :: String -> [ScmValue] -> ErrOr Bool
scmIsExact _ [ScmInt _]        = Right True
scmIsExact _ [ScmRat _]        = Right True
scmIsExact _ [x] | isNumeric x = Right False
scmIsExact s [_]               = notNum s
scmIsExact s _                 = tooManyArgs s

---- Comparison

scmCompare :: (forall a. Real a => a -> a -> Bool)
           -> String -> [ScmValue] -> ErrOr Bool
scmCompare _ s [] = tooFewArgs s
scmCompare p s xs = allM f . (zip`ap`tail) $ xs
 where
   f (a,b) = case liftScmRealA2 p a b of
                  Left _ ->
                     fail$ "Nonreal argument to primitive procedure " ++ s
                  x      -> x

scmIsZero :: [ScmValue] -> ErrOr Bool
scmIsZero [ScmInt     0]    = Right True
scmIsZero [ScmRat     0]    = Right True
scmIsZero [ScmReal    0]    = Right True
scmIsZero [ScmComplex 0]    = Right True
scmIsZero [x] | isNumeric x = Right False
scmIsZero [_]               = notNum "positive?"
scmIsZero _                 = tooManyArgs "zero?"

scmIsPos, scmIsNeg :: [ScmValue] -> ErrOr Bool
scmIsPos [x] = scmCompare (>) "positive?" [x, ScmInt 0]
scmIsPos _   = tooManyArgs "positive?"

scmIsNeg [x] = scmCompare (<) "negative?" [x, ScmInt 0]
scmIsNeg _   = tooManyArgs "negative?"

---- +-*/

scmPlus, scmMinus, scmMul, scmDiv :: [ScmValue] -> ErrOr ScmValue
scmPlus [] = Right $ ScmInt 0
scmPlus xs = foldM go (ScmInt 0) xs
 where
   go n x = if isNumeric x then liftScmNum2 (+) n x else notNum "+"

scmMinus []     = tooFewArgs "-"
scmMinus (x:xs) = if isNumeric x then foldM go x xs else notNum "-"
 where
   go n x = if isNumeric x then liftScmNum2 (-) n x else notNum "-"

scmMul [] = Right $ ScmInt 1
scmMul xs = foldM go (ScmInt 1) xs
 where
   go n x = if isNumeric x then liftScmNum2 (*) n x else notNum "*"

scmDiv []     = tooFewArgs "/"
scmDiv (x:xs) = unint x >>= flip (foldM go) xs
 where
   go n x = unint x >>= liftScmFrac2 (/) n

   unint (ScmInt x) = Right $ ScmRat (fromInteger x)
   unint x          = if isNumeric x then Right x else notNum "/"

notNum, tooFewArgs, tooManyArgs :: String -> ErrOr a
notNum      = fail . ("Nonnumeric argument to primitive procedure " ++)
tooFewArgs  = fail . ("Too few arguments to primitive procedure " ++)
tooManyArgs = fail . ("Too many arguments to primitive procedure " ++)

isNumeric :: ScmValue -> Bool
isNumeric (ScmInt     _) = True
isNumeric (ScmRat     _) = True
isNumeric (ScmReal    _) = True
isNumeric (ScmComplex _) = True
isNumeric _              = False

-- Lift ScmValue's numeric types to a common type
--
-- Versions without 'A' at the end also return results in the correct
-- constructor of the two given
--
-- Ugly and verbose... too lazy to metaize these
liftScmNum2 :: (forall a. Num a => a -> a -> a)
            -> (ScmValue -> ScmValue -> ErrOr ScmValue)
liftScmNum2 f (ScmInt     a) (ScmInt     b) = Right . ScmInt    $ f a b
liftScmNum2 f (ScmRat     a) (ScmRat     b) = Right . ScmRat    $ f a b
liftScmNum2 f (ScmReal    a) (ScmReal    b) = Right . ScmReal   $ f a b
liftScmNum2 f (ScmComplex a) (ScmComplex b) = Right . ScmComplex$ f a b

-- Int+{Rat,Real,Complex}
liftScmNum2 f (ScmInt     a) (ScmRat     b) = Right . ScmRat    $ f (fInt a) b
liftScmNum2 f (ScmRat     a) (ScmInt     b) = Right . ScmRat    $ f a (fInt b)
liftScmNum2 f (ScmInt     a) (ScmReal    b) = Right . ScmReal   $ f (fInt a) b
liftScmNum2 f (ScmReal    a) (ScmInt     b) = Right . ScmReal   $ f a (fInt b)
liftScmNum2 f (ScmInt     a) (ScmComplex b) = Right . ScmComplex$ f (fInt a) b
liftScmNum2 f (ScmComplex a) (ScmInt     b) = Right . ScmComplex$ f a (fInt b)

-- Rat+{Real,Complex}
liftScmNum2 f (ScmRat     a) (ScmReal    b) = Right . ScmReal   $ f (fRat a) b
liftScmNum2 f (ScmReal    a) (ScmRat     b) = Right . ScmReal   $ f a (fRat b)
liftScmNum2 f (ScmRat     a) (ScmComplex b) = Right . ScmComplex$ f (fRat a) b
liftScmNum2 f (ScmComplex a) (ScmRat     b) = Right . ScmComplex$ f a (fRat b)

-- Real+Complex
liftScmNum2 f (ScmReal    a) (ScmComplex b) = Right . ScmComplex$ f (a :+ 0) b
liftScmNum2 f (ScmComplex a) (ScmReal    b) = Right . ScmComplex$ f a (b :+ 0)

liftScmNum2 _ _ _ = fail "liftScmNum2 :: internal error"

liftScmFrac2 :: (forall a. Fractional a => a -> a -> a)
             -> (ScmValue -> ScmValue -> ErrOr ScmValue)
liftScmFrac2 f (ScmRat     a) (ScmRat     b) = Right . ScmRat    $ f a b
liftScmFrac2 f (ScmReal    a) (ScmReal    b) = Right . ScmReal   $ f a b
liftScmFrac2 f (ScmComplex a) (ScmComplex b) = Right . ScmComplex$ f a b

-- Rat+{Real,Complex}
liftScmFrac2 f (ScmRat     a) (ScmReal    b) = Right . ScmReal   $ f (fRat a) b
liftScmFrac2 f (ScmReal    a) (ScmRat     b) = Right . ScmReal   $ f a (fRat b)
liftScmFrac2 f (ScmRat     a) (ScmComplex b) = Right . ScmComplex$ f (fRat a) b
liftScmFrac2 f (ScmComplex a) (ScmRat     b) = Right . ScmComplex$ f a (fRat b)

-- Real+Complex
liftScmFrac2 f (ScmReal    a) (ScmComplex b) = Right . ScmComplex$ f (a :+ 0) b
liftScmFrac2 f (ScmComplex a) (ScmReal    b) = Right . ScmComplex$ f a (b :+ 0)

liftScmFrac2 _ _ _ = fail "liftScmFrac2 :: internal error"

liftScmRealA2 :: (forall a. Real a => a -> a -> b)
              -> (ScmValue -> ScmValue -> ErrOr b)
liftScmRealA2 _ (ScmComplex a) _ | imagPart a /= 0 =
   fail "liftScmRealA2 :: complex"

liftScmRealA2 _ _ (ScmComplex b) | imagPart b /= 0 =
   fail "liftScmRealA2 :: complex"

liftScmRealA2 f (ScmInt     a) (ScmInt     b) = Right $ f a b
liftScmRealA2 f (ScmRat     a) (ScmRat     b) = Right $ f a b
liftScmRealA2 f (ScmReal    a) (ScmReal    b) = Right $ f a b
liftScmRealA2 f (ScmComplex a) (ScmComplex b) = Right $ (f `on` realPart) a b

-- Int+{Rat,Real,Complex}
liftScmRealA2 f (ScmInt     a) (ScmRat     b) = Right $ f (fInt a) b
liftScmRealA2 f (ScmRat     a) (ScmInt     b) = Right $ f a (fInt b)
liftScmRealA2 f (ScmInt     a) (ScmReal    b) = Right $ f (fInt a) b
liftScmRealA2 f (ScmReal    a) (ScmInt     b) = Right $ f a (fInt b)
liftScmRealA2 f (ScmInt     a) (ScmComplex b) = Right $ f (fInt a) (realPart b)
liftScmRealA2 f (ScmComplex a) (ScmInt     b) = Right $ f (realPart a) (fInt b)

-- Rat+{Real,Complex}
liftScmRealA2 f (ScmRat     a) (ScmReal    b) = Right $ f (fRat a) b
liftScmRealA2 f (ScmReal    a) (ScmRat     b) = Right $ f a (fRat b)
liftScmRealA2 f (ScmRat     a) (ScmComplex b) = Right $ f (fRat a) (realPart b)
liftScmRealA2 f (ScmComplex a) (ScmRat     b) = Right $ f (realPart a) (fRat b)

-- Real+Complex
liftScmRealA2 f (ScmReal    a) (ScmComplex b) = Right $ f a (realPart b)
liftScmRealA2 f (ScmComplex a) (ScmReal    b) = Right $ f (realPart a) b

liftScmRealA2 _ _ _ = fail "liftScmRealA2 :: internal error"

fInt :: Num a => Integer -> a
fInt = fromInteger

fRat :: Fractional a => Rational -> a
fRat = fromRational
