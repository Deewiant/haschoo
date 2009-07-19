-- File created: 2009-07-11 21:47:19

module Haschoo.Datum where

import Data.Complex        (Complex((:+)))
import Data.Ratio          (numerator, denominator)
import Text.Show.Functions ()

import Haschoo.Utils (ErrOr, showScmList)

data Datum = Evaluated    ScmValue
           | Quoted       Datum
           | QuasiQuoted  Datum
           | UnQuoted     Datum
           | FlatUnQuoted Datum
           | UnevaledId   String
           | UnevaledApp  [Datum]
           | UnevaledVec  [Datum]
           | DottedList   [Datum] Datum
 deriving Show

data ScmValue = ScmPrim    String !([Datum]    -> ErrOr ScmValue)
              | ScmFunc    String !([ScmValue] -> ErrOr ScmValue)
              | ScmBool    !Bool
              | ScmChar    !Char
              | ScmString  !String
              | ScmInt     !Integer
              | ScmRat     !Rational
              | ScmReal    !Double
              | ScmComplex !(Complex Double)
              | ScmList    ![ScmValue]
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

scmShow (ScmString s) = '"' : foldr ((.) . f) id s "\""
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c

scmShowDatum :: Datum -> String
scmShowDatum (Evaluated v)     = scmShow v
scmShowDatum (Quoted x)        = '\'': scmShowDatum x
scmShowDatum (QuasiQuoted x)   = '`' : scmShowDatum x
scmShowDatum (UnQuoted x)      = ',' : scmShowDatum x
scmShowDatum (FlatUnQuoted x)  = ",@" ++ scmShowDatum x
scmShowDatum (UnevaledId s)    = s
scmShowDatum (UnevaledApp xs)  = showScmList scmShowDatum xs
scmShowDatum (UnevaledVec xs)  = '#' : showScmList scmShowDatum xs
scmShowDatum (DottedList xs x) =
   concat [init (showScmList scmShowDatum xs), " . ", scmShowDatum x, ")"]
