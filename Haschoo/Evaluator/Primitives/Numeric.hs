-- File created: 2009-07-15 21:15:44

{-# LANGUAGE Rank2Types #-}

module Haschoo.Evaluator.Primitives.Numeric (primitives) where

import Control.Arrow ((&&&))
import Data.Complex  (Complex((:+)))
import Data.List     (foldl', foldl1')

import Haschoo.ScmValue (ScmValue(..))
import Haschoo.Utils    (($<), (.:))

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("number?",   ScmBool . scmIsNumber)
   , ("complex?",  ScmBool . scmIsNumber)
   , ("real?",     ScmBool . scmIsReal)
   , ("rational?", ScmBool . scmIsRational)
   , ("integer?",  ScmBool . scmIsInteger)
   , "exact?"   $< id &&& ScmBool       .: scmIsExact
   , "inexact?" $< id &&& ScmBool . not .: scmIsExact
   , ("+", scmPlus)
   , ("-", scmMinus)
   , ("*", scmMul)
   , ("/", scmDiv)
   ]

scmIsNumber, scmIsReal, scmIsRational, scmIsInteger :: [ScmValue] -> Bool
scmIsNumber [x] = isNumeric x
scmIsNumber _   = tooManyArgs "number?"

scmIsReal [ScmComplex (_ :+ b)] = b == 0
scmIsReal [x]                   = isNumeric x
scmIsReal _                     = tooManyArgs "real?"

scmIsRational [ScmRat _] = True
scmIsRational [_]        = False
scmIsRational _          = tooManyArgs "rational?"

scmIsInteger [ScmInt     _]      = True
scmIsInteger [ScmRat     x]      = x == fromInteger (round x)
scmIsInteger [ScmReal    x]      = x == fromInteger (round x)
scmIsInteger [ScmComplex (a:+b)] = b == 0 && a == fromInteger (round a)
scmIsInteger [_]                 = False
scmIsInteger _                   = tooManyArgs "integer?"

scmIsExact :: String -> [ScmValue] -> Bool
scmIsExact _ [ScmInt _]        = True
scmIsExact _ [ScmRat _]        = True
scmIsExact _ [x] | isNumeric x = False
scmIsExact s [_]               = notNum s
scmIsExact s _                 = tooManyArgs s

scmPlus, scmMinus, scmMul, scmDiv :: [ScmValue] -> ScmValue
scmPlus [] = ScmInt 0
scmPlus xs = foldl1' go xs
 where
   go n x = if isNumeric x then liftScmNum2 (+) n x else notNum "+"

scmMinus [] = tooFewArgs "-"
scmMinus xs = foldl1' go xs
 where
   go n x = if isNumeric x then liftScmNum2 (-) n x else notNum "-"

scmMul [] = ScmInt 1
scmMul xs = foldl1' go xs
 where
   go n x = if isNumeric x then liftScmNum2 (*) n x else notNum "*"

scmDiv []     = tooFewArgs "/"
scmDiv (x:xs) = foldl' go (unint x) xs
 where
   go n x = if isNumeric x then liftScmFrac2 (/) n (unint x) else notNum "/"

   unint (ScmInt x) = ScmRat (fromInteger x)
   unint x          = x

notNum, tooFewArgs, tooManyArgs :: String -> a
notNum      = error . ("Nonnumeric argument to primitive procedure " ++)
tooFewArgs  = error . ("Too few arguments to primitive procedure " ++)
tooManyArgs = error . ("Too many arguments to primitive procedure " ++)

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
            -> (ScmValue -> ScmValue -> ScmValue)
liftScmNum2 f (ScmInt     a) (ScmInt     b) = ScmInt    $ f a b
liftScmNum2 f (ScmRat     a) (ScmRat     b) = ScmRat    $ f a b
liftScmNum2 f (ScmReal    a) (ScmReal    b) = ScmReal   $ f a b
liftScmNum2 f (ScmComplex a) (ScmComplex b) = ScmComplex$ f a b

-- Int+{Rat,Real,Complex}
liftScmNum2 f (ScmInt     a) (ScmRat     b) = ScmRat    $ f (fromInteger a) b
liftScmNum2 f (ScmRat     a) (ScmInt     b) = ScmRat    $ f a (fromInteger b)
liftScmNum2 f (ScmInt     a) (ScmReal    b) = ScmReal   $ f (fromInteger a) b
liftScmNum2 f (ScmReal    a) (ScmInt     b) = ScmReal   $ f a (fromInteger b)
liftScmNum2 f (ScmInt     a) (ScmComplex b) = ScmComplex$ f (fromInteger a) b
liftScmNum2 f (ScmComplex a) (ScmInt     b) = ScmComplex$ f a (fromInteger b)

-- Rat+{Real,Complex}
liftScmNum2 f (ScmRat     a) (ScmReal    b) = ScmReal   $ f (fromRational a) b
liftScmNum2 f (ScmReal    a) (ScmRat     b) = ScmReal   $ f a (fromRational b)
liftScmNum2 f (ScmRat     a) (ScmComplex b) = ScmComplex$ f (fromRational a) b
liftScmNum2 f (ScmComplex a) (ScmRat     b) = ScmComplex$ f a (fromRational b)

-- Real+Complex
liftScmNum2 f (ScmReal    a) (ScmComplex b) = ScmComplex$ f (a :+ 0) b
liftScmNum2 f (ScmComplex a) (ScmReal    b) = ScmComplex$ f a (b :+ 0)

liftScmNum2 _ _ _ = error "liftScmNum2 :: internal error"

liftScmFrac2 :: (forall a. Fractional a => a -> a -> a)
             -> (ScmValue -> ScmValue -> ScmValue)
liftScmFrac2 f (ScmRat     a) (ScmRat     b) = ScmRat    $ f a b
liftScmFrac2 f (ScmReal    a) (ScmReal    b) = ScmReal   $ f a b
liftScmFrac2 f (ScmComplex a) (ScmComplex b) = ScmComplex$ f a b

-- Rat+{Real,Complex}
liftScmFrac2 f (ScmRat     a) (ScmReal    b) = ScmReal   $ f (fromRational a) b
liftScmFrac2 f (ScmReal    a) (ScmRat     b) = ScmReal   $ f a (fromRational b)
liftScmFrac2 f (ScmRat     a) (ScmComplex b) = ScmComplex$ f (fromRational a) b
liftScmFrac2 f (ScmComplex a) (ScmRat     b) = ScmComplex$ f a (fromRational b)

-- Real+Complex
liftScmFrac2 f (ScmReal    a) (ScmComplex b) = ScmComplex$ f (a :+ 0) b
liftScmFrac2 f (ScmComplex a) (ScmReal    b) = ScmComplex$ f a (b :+ 0)

liftScmFrac2 _ _ _ = error "liftScmFrac2 :: internal error"
