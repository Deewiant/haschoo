-- File created: 2009-07-21 10:36:31

module Haschoo.Evaluator.Standard.Equivalence
   (procedures, scmEq, scmEqv, scmEqual) where

import Control.Arrow     ((&&&))
import Control.Monad     (liftM2)
import Data.Array.MArray (getElems)

import Haschoo.Types                      (ScmValue(..), pairToList)
import Haschoo.Utils                      (ErrOr, ($<), (.:), eqWithM, ptrEq)
import Haschoo.Evaluator.Utils            (tooFewArgs, tooManyArgs)
import Haschoo.Evaluator.Standard.Numeric (isExact, isNumeric, numEq)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b))
   [ "eq?"    $< id &&& fmap (fmap ScmBool) .: scmEquivalence scmEq
   , "eqv?"   $< id &&& fmap (fmap ScmBool) .: scmEquivalence scmEqv
   , "equal?" $< id &&& fmap (fmap ScmBool) .: scmEquivalence scmEqual
   ]

scmEq, scmEqv, scmEqual :: ScmValue -> ScmValue -> IO Bool
scmEqv (ScmBool       a) (ScmBool       b) = return$ a == b
scmEqv (ScmIdentifier a) (ScmIdentifier b) = return$ a == b
scmEqv (ScmChar       a) (ScmChar       b) = return$ a == b
scmEqv a b =
   if isNumeric a && isNumeric b
      then return$ isExact a == isExact b && numEq a b
      else ptrEq a b

scmEq a b = if isNumeric a && isNumeric b
               then ptrEq  a b
               else scmEqv a b

scmEqual (ScmList       x)   (ScmList       y)   = eqWithM scmEqual x y
scmEqual (ScmDottedList x a) (ScmDottedList y b) = do
   ab <- scmEqual a b
   if ab
      then eqWithM scmEqual x y
      else return False

scmEqual x@(ScmList _) y@(ScmPair _ _) =
   either (const $ return False) (scmEqual x) =<< pairToList y

scmEqual x@(ScmDottedList _ _) y@(ScmPair _ _) =
   either (scmEqual x) (const $ return False) =<< pairToList y

scmEqual y@(ScmPair _ _) x@(ScmList       _)   = scmEqual x y
scmEqual y@(ScmPair _ _) x@(ScmDottedList _ _) = scmEqual x y

scmEqual (ScmString  a) (ScmString  b) = return$ a == b
scmEqual (ScmMString a) (ScmMString b) = liftM2 (==) (getElems a) (getElems b)
scmEqual a b = scmEqv a b

scmEquivalence :: (ScmValue -> ScmValue -> IO Bool) -> String
               -> [ScmValue] -> IO (ErrOr Bool)

scmEquivalence f _ [x,y]   = fmap Right $ f x y
scmEquivalence _ s (_:_:_) = return$ tooManyArgs s
scmEquivalence _ s _       = return$ tooFewArgs  s
