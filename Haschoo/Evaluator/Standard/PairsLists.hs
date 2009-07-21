-- File created: 2009-07-21 22:13:29

module Haschoo.Evaluator.Standard.PairsLists (procedures) where

import Haschoo.Types           (ScmValue(..))
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
scmCons [a, ScmList       b]   = Right (ScmList       (a:b))
scmCons [a, ScmDottedList b c] = Right (ScmDottedList (a:b) c)
scmCons [a, b]                 = Right (ScmPair a b)
scmCons (_:_:_)                = tooManyArgs "cons"
scmCons _                      = tooFewArgs  "cons"

scmCar, scmCdr :: [ScmValue] -> ErrOr ScmValue
scmCar [ScmPair a _]           = Right a
scmCar [ScmList (a:_)]         = Right a
scmCar [ScmDottedList (a:_) _] = Right a
scmCar [ScmList []]            = fail "car :: empty list"
scmCar [_]                     = fail "Nonpair argument to car"
scmCar []                      = tooFewArgs  "car"
scmCar _                       = tooManyArgs "car"

scmCdr [ScmPair _ b]           = Right b
scmCdr [ScmList (_:bs)]        = Right $ ScmList bs
scmCdr [ScmDottedList [_]   b] = Right b
scmCdr [ScmDottedList (_:a) b] = Right $ ScmDottedList a b
scmCdr [ScmList []]            = fail "cdr :: empty list"
scmCdr [_]                     = fail "Nonpair argument to cdr"
scmCdr []                      = tooFewArgs  "cdr"
scmCdr _                       = tooManyArgs "cdr"
