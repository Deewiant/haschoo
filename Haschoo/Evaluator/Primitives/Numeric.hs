-- File created: 2009-07-15 21:15:44

module Haschoo.Evaluator.Primitives.Numeric (primitives) where

import Control.Arrow ((&&&))
import Data.Complex  (Complex((:+)))
import Data.List     (foldl', foldl1')

import Haschoo.ScmValue (ScmValue(..), isNumeric, liftScmFrac2, liftScmNum2)
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
