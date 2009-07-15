-- File created: 2009-07-11 21:47:19

module Haschoo.ScmValue where

import Haschoo.Utils (showScmList)

data ScmValue = ScmFunc   String ([ScmValue] -> ScmValue)
              | ScmBool   Bool
              | ScmChar   Char
              | ScmString String
              | ScmInt    Integer
              | ScmList   [ScmValue]

instance Show ScmValue where
   showsPrec d (ScmFunc s _) = showParen (d>10) $
      showString "ScmFunc " . showsPrec (d+1) s
   showsPrec d (ScmBool b)   = showParen (d>10) $
      showString "ScmBool " . showsPrec (d+1) b
   showsPrec d (ScmChar b)   = showParen (d>10) $
      showString "ScmChar " . showsPrec (d+1) b
   showsPrec d (ScmString b) = showParen (d>10) $
      showString "ScmString " . showsPrec (d+1) b
   showsPrec d (ScmInt b)    = showParen (d>10) $
      showString "ScmInt " . showsPrec (d+1) b
   showsPrec d (ScmList b)   = showParen (d>10) $
      showString "ScmList " . showsPrec (d+1) b

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
