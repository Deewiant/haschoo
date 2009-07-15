-- File created: 2009-07-11 21:47:19

module Haschoo.ScmValue where

import Text.Show.Functions ()

import Haschoo.Utils (showScmList)

data ScmValue = ScmFunc   String ([ScmValue] -> ScmValue)
              | ScmBool   Bool
              | ScmChar   Char
              | ScmString String
              | ScmInt    Integer
              | ScmList   [ScmValue]
 deriving Show

scmShow :: ScmValue -> String
scmShow (ScmBool b)             = '#' : if b then "t" else "f"
scmShow (ScmInt n)              = show n
scmShow (ScmFunc s _)           = s
scmShow (ScmList xs)            = showScmList scmShow xs
scmShow (ScmChar c) | c == ' '  = "#\\space"
                    | c == '\n' = "#\\newline"
                    | otherwise = "#\\" ++ [c]
scmShow (ScmString s) = foldr ((.) . f) id s s
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c
