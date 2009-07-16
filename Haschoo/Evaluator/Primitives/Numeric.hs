-- File created: 2009-07-15 21:15:44

{-# LANGUAGE FlexibleInstances, Rank2Types, TypeSynonymInstances #-}

module Haschoo.Evaluator.Primitives.Numeric (primitives) where

import Control.Applicative ((<$>))
import Control.Arrow       ((&&&))
import Control.Monad       (ap, foldM)
import Data.Complex        (Complex((:+)), imagPart, realPart)
import Data.Ratio          (numerator, denominator)
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
   , "even?" $< id &&& fmap  ScmBool        .: scmIsEven
   , "odd?"  $< id &&& fmap (ScmBool . not) .: scmIsEven

   , ("max", scmMax)
   , ("min", scmMin)

   , ("+", scmPlus)
   , ("-", scmMinus)
   , ("*", scmMul)
   , ("/", scmDiv)

   , ("quotient",  scmQuot)
   , ("remainder", scmRem)
   , ("modulo",    scmMod)
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

scmIsInteger [x] = Right $ isInteger x
scmIsInteger _   = tooManyArgs "integer?"

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

scmIsEven :: String -> [ScmValue] -> ErrOr Bool
scmIsEven s [x] = if isInteger x
                     then (scmIsZero.return) =<< scmMod [x, ScmInt 2]
                     else notInt s
scmIsEven s _   = tooManyArgs s

scmMax, scmMin :: [ScmValue] -> ErrOr ScmValue
scmMax []     = tooFewArgs "max"
scmMax (x:xs) = if isNumeric x then foldM go x xs else notNum "max"
 where
   go n x = if isNumeric x then liftScmReal2 max n x else notNum "max"

scmMin []     = tooFewArgs "min"
scmMin (x:xs) = if isNumeric x then foldM go x xs else notNum "min"
 where
   go n x = if isNumeric x then liftScmReal2 min n x else notNum "min"

---- +-*/

scmPlus, scmMinus, scmMul, scmDiv :: [ScmValue] -> ErrOr ScmValue
scmPlus [] = Right $ ScmInt 0
scmPlus xs = foldM go (ScmInt 0) xs
 where
   go n x = if isNumeric x then liftScmNum2 (+) n x else notNum "+"

scmMinus []                        = tooFewArgs "-"
scmMinus (x:_) | not (isNumeric x) = notNum "-"
scmMinus [x]                       = liftScmNum negate x
scmMinus (x:xs)                    = foldM go x xs
 where
   go n x = if isNumeric x then liftScmNum2 (-) n x else notNum "-"

scmMul [] = Right $ ScmInt 1
scmMul xs = foldM go (ScmInt 1) xs
 where
   go n x = if isNumeric x then liftScmNum2 (*) n x else notNum "*"

scmDiv []     = tooFewArgs "/"
scmDiv (x:xs) =
   unint x >>= case xs of
                    []  -> liftScmFrac recip
                    _:_ -> flip (foldM go) xs
 where
   go n x = unint x >>= liftScmFrac2 (/) n

   unint (ScmInt x) = Right $ ScmRat (fromInteger x)
   unint x          = if isNumeric x then Right x else notNum "/"

---- quot rem mod

scmQuot, scmRem, scmMod :: [ScmValue] -> ErrOr ScmValue
scmQuot = scmQuotRemMod quot "quotient"
scmRem  = scmQuotRemMod rem "remainder"
scmMod  = scmQuotRemMod mod "modulo"

scmQuotRemMod :: (Integer -> Integer -> Integer) -> String
              -> [ScmValue] -> ErrOr ScmValue
scmQuotRemMod f s [x,y] = do
   (e1,a) <- asInt s x
   (e2,b) <- asInt s y
   return . (if e1 && e2 then ScmInt else ScmReal . fromInteger) $ f a b
scmQuotRemMod _ s (_:_:_) = tooManyArgs s
scmQuotRemMod _ s _       = tooFewArgs  s

-------------

notInt, notNum, tooFewArgs, tooManyArgs :: String -> ErrOr a
notInt      = fail . ("Noninteger argument to primitive procedure " ++)
notNum      = fail . ("Nonnumeric argument to primitive procedure " ++)
tooFewArgs  = fail . ("Too few arguments to primitive procedure " ++)
tooManyArgs = fail . ("Too many arguments to primitive procedure " ++)

isNumeric, isInteger :: ScmValue -> Bool
isNumeric (ScmInt     _) = True
isNumeric (ScmRat     _) = True
isNumeric (ScmReal    _) = True
isNumeric (ScmComplex _) = True
isNumeric _              = False

isInteger (ScmInt     _)      = True
isInteger (ScmRat     x)      = denominator x == 1
isInteger (ScmReal    x)      = x == fromInteger (round x)
isInteger (ScmComplex (a:+b)) = b == 0 && a == fromInteger (round a)
isInteger _                   = False

asInt :: String -> ScmValue -> ErrOr (Bool, Integer)
asInt s x | not (isInteger x) = notInt s
asInt _ (ScmInt     x)        = Right (True,  x)
asInt _ (ScmRat     x)        = Right (True,  numerator x)
asInt _ (ScmReal    x)        = Right (False, round x)
asInt _ (ScmComplex (x:+0))   = Right (False, round x)

-- Lift ScmValue's numeric types to a common type
--
-- Versions without 'A' at the end also return results in the correct
-- constructor of the two given
--
-- Ugly and verbose... too lazy to metaize these

--- Num
liftScmNum :: (forall a. Num a => a -> a) -> ScmValue -> ErrOr ScmValue
liftScmNum f (ScmInt     a) = Right . ScmInt     $ f a
liftScmNum f (ScmRat     a) = Right . ScmRat     $ f a
liftScmNum f (ScmReal    a) = Right . ScmReal    $ f a
liftScmNum f (ScmComplex a) = Right . ScmComplex $ f a
liftScmNum _ _ = fail "liftScmNum :: internal error"

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

--- Frac
liftScmFrac :: (forall a. Fractional a => a -> a) -> ScmValue -> ErrOr ScmValue
liftScmFrac f (ScmRat     a) = Right . ScmRat     $ f a
liftScmFrac f (ScmReal    a) = Right . ScmReal    $ f a
liftScmFrac f (ScmComplex a) = Right . ScmComplex $ f a
liftScmFrac _ _ = fail "liftScmFrac :: internal error"

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

--- Real
liftScmReal2 :: (forall a. Real a => a -> a -> a)
             -> (ScmValue -> ScmValue -> ErrOr ScmValue)
liftScmReal2 f (ScmInt     a) (ScmInt     b) = Right . ScmInt    $ f a b
liftScmReal2 f (ScmRat     a) (ScmRat     b) = Right . ScmRat    $ f a b
liftScmReal2 f (ScmReal    a) (ScmReal    b) = Right . ScmReal   $ f a b

-- Int+{Rat,Real}
liftScmReal2 f (ScmInt     a) (ScmRat     b) = Right . ScmRat    $ f (fInt a) b
liftScmReal2 f (ScmRat     a) (ScmInt     b) = Right . ScmRat    $ f a (fInt b)
liftScmReal2 f (ScmInt     a) (ScmReal    b) = Right . ScmReal   $ f (fInt a) b
liftScmReal2 f (ScmReal    a) (ScmInt     b) = Right . ScmReal   $ f a (fInt b)

-- Rat+Real
liftScmReal2 f (ScmRat     a) (ScmReal    b) = Right . ScmReal   $ f (fRat a) b
liftScmReal2 f (ScmReal    a) (ScmRat     b) = Right . ScmReal   $ f a (fRat b)

liftScmReal2 _ _ _ = fail "liftScmReal2 :: internal error"

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
