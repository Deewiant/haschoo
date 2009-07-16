-- File created: 2009-07-15 21:15:44

{-# LANGUAGE Rank2Types #-}

module Haschoo.Evaluator.Primitives.Numeric (primitives) where

import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.Complex  (Complex((:+)))

import Haschoo.ScmValue (ScmValue(..))
import Haschoo.Utils    (ErrOr, ($<), (.:))

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("number?",   fmap ScmBool . scmIsNumber)
   , ("complex?",  fmap ScmBool . scmIsNumber)
   , ("real?",     fmap ScmBool . scmIsReal)
   , ("rational?", fmap ScmBool . scmIsRational)
   , ("integer?",  fmap ScmBool . scmIsInteger)
   , "exact?"   $< id &&& fmap  ScmBool        .: scmIsExact
   , "inexact?" $< id &&& fmap (ScmBool . not) .: scmIsExact
   , ("+", scmPlus)
   , ("-", scmMinus)
   , ("*", scmMul)
   , ("/", scmDiv)
   ]

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
scmDiv (x:xs) = foldM go (unint x) xs
 where
   go n x = if isNumeric x then liftScmFrac2 (/) n (unint x) else notNum "/"

   unint (ScmInt x) = ScmRat (fromInteger x)
   unint x          = x

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

-- Return results with the more inexact constructor of the two given
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

fInt :: Num a => Integer -> a
fInt = fromInteger

fRat :: Fractional a => Rational -> a
fRat = fromRational
