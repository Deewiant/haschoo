-- File created: 2009-07-15 11:00:10

{-# LANGUAGE Rank2Types #-}

module Haschoo.Evaluator.Primitives (primitives) where

import Data.List (foldl')

import Haschoo.ScmValue (ScmValue(..), isNumeric, liftScmFrac2, liftScmNum2)

primitives :: [(String, ScmValue)]
primitives = [ scmFoldNum  "+" (+)
             , scmFoldNum  "-" (-)
             , scmFoldNum  "*" (*)
             , scmFoldFrac "/" (/)
             ]

scmFoldNum :: String -> (forall a. Num a => a -> a -> a) -> (String, ScmValue)
scmFoldNum s f = (s, ScmFunc s start)
 where
   start []     = error $ "Wanted arguments for primitive " ++ s
   start (x:xs) =
      if isNumeric x
         then foldl' go x xs
         else badArg

   go n x =
      if isNumeric x
         then liftScmNum2 f n x
         else badArg

   badArg = error $ "Nonnumeric argument to primitive " ++ s

-- Like scmFoldNum but raises ScmInts to ScmRats
scmFoldFrac :: String -> (forall a. Fractional a => a -> a -> a)
            -> (String, ScmValue)
scmFoldFrac s f = (s, ScmFunc s start)
 where
   start []               = error $ "Wanted arguments for primitive " ++ s
   start (x:_) | notNum x = badArg
   start (ScmInt x:xs)    = start (ScmRat (fromInteger x) : xs)
   start (x:xs)           = foldl' go x xs

   go _ x | notNum x = badArg
   go n (ScmInt x)   = go n (ScmRat (fromInteger x))
   go n x            =
      if isNumeric x
         then liftScmFrac2 f n x
         else badArg

   notNum = not . isNumeric
   badArg = error $ "Nonnumeric argument to primitive " ++ s
