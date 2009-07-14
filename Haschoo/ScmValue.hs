-- File created: 2009-07-11 21:47:19

module Haschoo.ScmValue where

import Data.List (intercalate)

-- The list types:
--   Application: (a b c)
--   DottedList:  (a b . c)
--   ScmList:     (list a b c)
data ScmValue = UnevaledId  String
              | Application [ScmValue]
              | DottedList  [ScmValue] ScmValue
              | ScmBool     Bool
              | ScmChar     Char
              | ScmString   String
              | ScmInt      Integer
              | ScmList     [ScmValue]
              | ScmVector   [ScmValue]
              | ScmQuoted   ScmValue
 deriving Show

scmShow :: ScmValue -> String
scmShow (UnevaledId   s)        = s
scmShow (Application xs)        = scmShowList xs
scmShow (DottedList  xs x)      =
   concat ["(", intercalate " " (map scmShow xs), " . ", scmShow x, ")"]
scmShow (ScmBool b)             = '#' : if b then "t" else "f"
scmShow (ScmInt n)              = show n
scmShow (ScmList xs)            = scmShowList xs
scmShow (ScmVector xs)          = '#' : scmShowList xs
scmShow (ScmQuoted x)           = '\'' : scmShow x
scmShow (ScmChar c) | c == ' '  = "#\\space"
                    | c == '\n' = "#\\newline"
                    | otherwise = "#\\" ++ [c]
scmShow (ScmString s) = foldr ((.) . f) id s s
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c

scmShowList :: [ScmValue] -> String
scmShowList xs = concat ["(", intercalate " " (map scmShow xs), ")"]
