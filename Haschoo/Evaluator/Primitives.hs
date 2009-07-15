-- File created: 2009-07-15 11:00:10

module Haschoo.Evaluator.Primitives (primitives) where

import Data.List (foldl', foldl1')

import Haschoo.ScmValue (ScmValue(..), isNumeric, liftScmFrac2, liftScmNum2)

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("+", scmPlus)
   , ("-", scmMinus)
   , ("*", scmMul)
   , ("/", scmDiv)
   ]

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

notNum, tooFewArgs :: String -> a
notNum     = error . ("Nonnumeric argument to primitive procedure " ++)
tooFewArgs = error . ("No arguments to primitive procedure " ++)
