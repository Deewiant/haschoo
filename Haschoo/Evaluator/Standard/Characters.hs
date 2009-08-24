-- File created: 2009-08-04 19:35:58

module Haschoo.Evaluator.Standard.Characters (procedures) where

import Control.Arrow ((&&&))
import Data.Char     ( isAlpha, isDigit, isSpace, isLower, isUpper
                     , isLetter, toLower, toUpper)

import Haschoo.Types           (ScmValue(..))
import Haschoo.Utils           (ErrOr, ($<), (.:))
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs, notInt, notChar)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a (return . b)))
   [ ("char?",  fmap ScmBool . scmIsChar)

   , "char=?"  $< id &&& fmap ScmBool .: scmCompare (==) (,)
   , "char<?"  $< id &&& fmap ScmBool .: scmCompare (<)  (,)
   , "char>?"  $< id &&& fmap ScmBool .: scmCompare (>)  (,)
   , "char<=?" $< id &&& fmap ScmBool .: scmCompare (<=) (,)
   , "char>=?" $< id &&& fmap ScmBool .: scmCompare (>=) (,)

   , "char-ci=?"  $< id &&& fmap ScmBool .: scmCompare (==) lower
   , "char-ci<?"  $< id &&& fmap ScmBool .: scmCompare (<)  lower
   , "char-ci>?"  $< id &&& fmap ScmBool .: scmCompare (>)  lower
   , "char-ci<=?" $< id &&& fmap ScmBool .: scmCompare (<=) lower
   , "char-ci>=?" $< id &&& fmap ScmBool .: scmCompare (>=) lower

   , "char-alphabetic?" $< id &&& fmap ScmBool .: scmProperty isAlpha
   , "char-numeric?"    $< id &&& fmap ScmBool .: scmProperty isDigit
   , "char-whitespace?" $< id &&& fmap ScmBool .: scmProperty isSpace
   , "char-upper-case?" $< id &&& fmap ScmBool .: scmProperty isUpper
   , "char-lower-case?" $< id &&& fmap ScmBool .: scmProperty isLower

   , ("char->integer", scmToInt)
   , ("integer->char", scmToChar)

   , "char-upcase"   $< id &&& fmap ScmChar .: scmApply toUpper
   , "char-downcase" $< id &&& fmap ScmChar .: scmApply toLower
   ]
 where
   lower a b = if isLetter a && isLetter b
                  then (toLower a, toLower b)
                  else (a,b)

scmIsChar :: [ScmValue] -> ErrOr Bool
scmIsChar [ScmChar _] = Right True
scmIsChar [_]         = Right False
scmIsChar []          = tooFewArgs  "char?"
scmIsChar _           = tooManyArgs "char?"

scmCompare :: (Char -> Char -> Bool) -> (Char -> Char -> (Char,Char)) -> String
           -> [ScmValue] -> ErrOr Bool
scmCompare p f _ [ScmChar a, ScmChar b] = Right . uncurry p $ f a b
scmCompare _ _ s [_,_]                  = notChar     s
scmCompare _ _ s (_:_:_)                = tooManyArgs s
scmCompare _ _ s _                      = tooFewArgs  s

scmProperty :: (Char -> Bool) -> String -> [ScmValue] -> ErrOr Bool
scmProperty f _ [ScmChar c] = Right $ f c
scmProperty _ s [_]         = notChar     s
scmProperty _ s []          = tooFewArgs  s
scmProperty _ s _           = tooManyArgs s

scmToInt, scmToChar :: [ScmValue] -> ErrOr ScmValue
scmToInt [ScmChar c] = Right . ScmInt . toInt $ c
scmToInt [_]         = notChar     "char->integer"
scmToInt []          = tooFewArgs  "char->integer"
scmToInt _           = tooManyArgs "char->integer"

scmToChar [ScmInt i] =
   if i >= toInt (minBound :: Char) && i <= toInt (maxBound :: Char)
      then Right . ScmChar . toChar $ i
      else fail "Out-of-range integer to integer->char"

scmToChar [_]        = notInt      "integer->char"
scmToChar []         = tooFewArgs  "integer->char"
scmToChar _          = tooManyArgs "integer->char"

scmApply :: (Char -> Char) -> String -> [ScmValue] -> ErrOr Char
scmApply f _ [ScmChar c] = Right $ f c
scmApply _ s [_]         = notChar     s
scmApply _ s []          = tooFewArgs  s
scmApply _ s _           = tooManyArgs s

------

toChar :: Integer -> Char
toChar = toEnum . fromInteger

toInt :: Char -> Integer
toInt = toInteger . fromEnum
