-- File created: 2009-07-11 21:47:19

{-# LANGUAGE Rank2Types #-}

module Haschoo.ScmValue where

import Data.Complex        (Complex((:+)))
import Data.Ratio          (numerator, denominator)
import Text.Show.Functions ()

import Haschoo.Utils (showScmList)

data ScmValue = ScmFunc    String ([ScmValue] -> ScmValue)
              | ScmBool    Bool
              | ScmChar    Char
              | ScmString  String
              | ScmInt     Integer
              | ScmRat     Rational
              | ScmReal    Double
              | ScmComplex (Complex Double)
              | ScmList    [ScmValue]
 deriving Show

isNumeric :: ScmValue -> Bool
isNumeric (ScmInt     _) = True
isNumeric (ScmRat     _) = True
isNumeric (ScmReal    _) = True
isNumeric (ScmComplex _) = True
isNumeric _              = False

scmShow :: ScmValue -> String
scmShow (ScmBool b)   = '#' : if b then "t" else "f"
scmShow (ScmFunc s _) = s
scmShow (ScmList xs)  = showScmList scmShow xs
scmShow (ScmInt n)    = show n
scmShow (ScmReal n)   = show n
scmShow (ScmRat n)    =
   concat [show (numerator n), "/", show (denominator n)]

scmShow (ScmComplex (a :+ b)) =
   show a ++ concat (if b < 0
                        then ["-", show (-b), "i"]
                        else ["+", show b])

scmShow (ScmChar c) | c == ' '  = "#\\space"
                    | c == '\n' = "#\\newline"
                    | otherwise = "#\\" ++ [c]

scmShow (ScmString s) = foldr ((.) . f) id s s
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c

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
