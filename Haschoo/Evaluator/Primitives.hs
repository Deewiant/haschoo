-- File created: 2009-07-15 11:00:10

module Haschoo.Evaluator.Primitives (primitives) where

import Data.List (foldl')

import Haschoo.ScmValue (ScmValue(..))

primitives :: [(String, ScmValue)]
primitives = [ scmFoldNum "+" (+)
             , scmFoldNum "-" (-)
             , scmFoldNum "*" (*)
             , scmFoldNum "/" div
             ]

scmFoldNum :: String -> (Integer -> Integer -> Integer) -> (String, ScmValue)
scmFoldNum s f = (s, ScmFunc s $ ScmInt . start)
 where
   start []            = error $ "Wanted arguments for primitive " ++ s
   start (ScmInt x:xs) = foldl' go x xs
   start _             = badArg

   go n (ScmInt x) = f n x
   go _ _          = badArg

   badArg = error $ "Nonnumeric argument to primitive " ++ s
