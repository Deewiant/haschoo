-- File created: 2009-07-21 10:36:31

module Haschoo.Evaluator.Standard.Equivalence
   (procedures, scmEq, scmEqv, scmEqual) where

import Control.Arrow       ((&&&))
import Control.Monad       (liftM2, join)
import Control.Monad.Loops (allM)
import Data.Array.IArray   (bounds, elems)
import Data.Array.MArray   (getBounds, getElems, unsafeFreeze)
import Data.Function       (on)

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
scmEqv (ScmList       a) (ScmList       b) = ptrEq a b
scmEqv (ScmString     a) (ScmString     b) = ptrEq a b
scmEqv (ScmMString    a) (ScmMString    b) = ptrEq a b
scmEqv (ScmVector     a) (ScmVector     b) = ptrEq a b
scmEqv (ScmMVector    a) (ScmMVector    b) = ptrEq a b

scmEqv (ScmDottedList a b) (ScmDottedList x y) =
   liftM2 (&&) (ptrEq a x) (ptrEq b y)

scmEqv (ScmPair a b) (ScmPair x y) = liftM2 (&&) (ptrEq a x) (ptrEq b y)
scmEqv a b =
   if isNumeric a && isNumeric b
      then return$ isExact a == isExact b && numEq a b
      else ptrEq a b

scmEq a b = if isNumeric a && isNumeric b
               then ptrEq  a b
               else scmEqv a b

scmEqual x@(ScmPair _ _) y@(ScmPair _ _) =
   join $ (liftM2 scmEqual `on` fmap (either id id) . pairToList) x y

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
scmEqual (ScmString  a) (ScmMString b) = fmap (elems a==) $ getElems b
scmEqual (ScmMString a) (ScmString  b) = fmap (==elems b) $ getElems a

scmEqual (ScmVector  a) (ScmVector  b) =
   if bounds a == bounds b
      then allM (uncurry scmEqual) $ (zip`on`elems) a b
      else return False

scmEqual (ScmMVector a) (ScmMVector b) = do
   ba <- getBounds a
   bb <- getBounds b
   if ba == bb
      then join $ (liftM2 (allM (uncurry scmEqual) .: zip) `on` getElems) a b
      else return False

scmEqual a@(ScmVector _)  (ScmMVector b) =
   unsafeFreeze b >>= scmEqual a . ScmVector
scmEqual  (ScmMVector a) b@(ScmVector _) =
   unsafeFreeze a >>= scmEqual b . ScmVector

scmEqual a b = scmEqv a b

scmEquivalence :: (ScmValue -> ScmValue -> IO Bool) -> String
               -> [ScmValue] -> IO (ErrOr Bool)

scmEquivalence f _ [x,y]   = fmap Right $ f x y
scmEquivalence _ s (_:_:_) = return$ tooManyArgs s
scmEquivalence _ s _       = return$ tooFewArgs  s
