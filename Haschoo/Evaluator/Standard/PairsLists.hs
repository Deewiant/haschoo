-- File created: 2009-07-21 22:13:29

module Haschoo.Evaluator.Standard.PairsLists (procedures) where

import Haschoo.Types           (ScmValue(..), Datum(..))
import Haschoo.Utils           (ErrOr)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("pair?", return . fmap ScmBool . scmIsPair)
   , ("cons",  return . scmCons)
   , ("car",   return . scmCar)
   , ("cdr",   return . scmCdr) ]

scmIsPair :: [ScmValue] -> ErrOr Bool
scmIsPair [ScmPair _ _] = Right True
scmIsPair [ScmList _]   = Right True
scmIsPair [_]           = Right False
scmIsPair []            = tooFewArgs  "pair?"
scmIsPair _             = tooManyArgs "pair?"

scmCons :: [ScmValue] -> ErrOr ScmValue
scmCons [a,b]   = Right (ScmPair a b)
scmCons (_:_:_) = tooManyArgs "cons"
scmCons _       = tooFewArgs  "cons"

-- XXX: Our way of representing symbols seems to be painfully poor
scmCar, scmCdr :: [ScmValue] -> ErrOr ScmValue
scmCar [ScmPair a _]                    = Right a
scmCar [ScmList []]                     = fail "car :: empty list"
scmCar [ScmList (a:_)]                  = Right a
scmCar [ScmQuoted (UnevaledApp [])]     = fail "car :: empty list"
scmCar [ScmQuoted (UnevaledApp (d:_))]  = Right . ScmQuoted$ d
scmCar [ScmQuoted (DottedList (d:_) _)] = Right . ScmQuoted$ d
scmCar [ScmQuoted _]                    = Right . ScmQuoted$ UnevaledId "quote"
scmCar [_]                              = fail "Nonpair argument to car"
scmCar []                               = tooFewArgs  "car"
scmCar _                                = tooManyArgs "car"

scmCdr [ScmPair _ b]                    = Right b
scmCdr [ScmList []]                     = fail "cdr :: empty list"
scmCdr [ScmList (_:bs)]                 = Right $ ScmList bs
scmCdr [ScmQuoted (UnevaledApp [])]     = fail "cdr :: empty list"
scmCdr [ScmQuoted (UnevaledApp (_:ds))] = Right $ ScmList (map ScmQuoted ds)
scmCdr [ScmQuoted (DottedList [_]   b)] = Right . ScmQuoted $ b
scmCdr [ScmQuoted (DottedList (_:a) b)] = Right . ScmQuoted $ DottedList a b
scmCdr [_]                              = fail "Nonpair argument to cdr"
scmCdr []                               = tooFewArgs  "cdr"
scmCdr _                                = tooManyArgs "cdr"
