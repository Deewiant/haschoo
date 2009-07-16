-- File created: 2009-07-11 21:47:19

module Haschoo.ScmValue where

import Data.Complex        (Complex((:+)))
import Data.Ratio          (numerator, denominator)
import Text.Show.Functions ()

import Haschoo.Utils (ErrOr, showScmList)

data ScmValue = ScmFunc    String ([ScmValue] -> ErrOr ScmValue)
              | ScmBool    Bool
              | ScmChar    Char
              | ScmString  String
              | ScmInt     Integer
              | ScmRat     Rational
              | ScmReal    Double
              | ScmComplex (Complex Double)
              | ScmList    [ScmValue]
 deriving Show

scmShow :: ScmValue -> String
scmShow (ScmBool b)   = '#' : if b then "t" else "f"
scmShow (ScmFunc s _) = s
scmShow (ScmList xs)  = showScmList scmShow xs
scmShow (ScmInt n)    = show n
scmShow (ScmReal n)
   | isNaN      n = "+nan.#"
   | isInfinite n = (if n < 0 then '-' else '+') : "inf.#"
   | otherwise    = show n
scmShow (ScmRat n)    =
   concat [show (numerator n), "/", show (denominator n)]

scmShow (ScmComplex (a :+ b)) =
   let bs = scmShow (ScmReal b)
    in concat [ scmShow (ScmReal a)
              , if head bs `elem` "-+" then [] else "+"
              , bs
              , "i" ]

scmShow (ScmChar c) | c == ' '  = "#\\space"
                    | c == '\n' = "#\\newline"
                    | otherwise = "#\\" ++ [c]

scmShow (ScmString s) = foldr ((.) . f) id s s
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c
