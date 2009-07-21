-- File created: 2009-07-21 10:36:31

module Haschoo.Evaluator.Standard.Equivalence (procedures) where

import Control.Arrow         ((&&&))
import System.Mem.StableName (makeStableName)

import Haschoo.Types                      (ScmValue(..))
import Haschoo.Utils                      (ErrOr, ($<), (.:))
import Haschoo.Evaluator.Utils            (tooFewArgs, tooManyArgs)
import Haschoo.Evaluator.Standard.Numeric (isExact, isNumeric, numEq)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ "eq?"    $< id &&& fmap (fmap ScmBool) .: scmEquivalence scmEq
   , "eqv?"   $< id &&& fmap (fmap ScmBool) .: scmEquivalence scmEqv
   , "equal?" $< id &&& fmap (fmap ScmBool) .: scmEquivalence scmEqual
   ]

scmEq, scmEqv, scmEqual :: ScmValue -> ScmValue -> IO Bool
scmEqv   (ScmBool         a)   (ScmBool         b) = return$ a == b
scmEqv   (ScmIdentifier   a)   (ScmIdentifier   b) = return$ a == b
scmEqv   (ScmChar         a)   (ScmChar         b) = return$ a == b
scmEqv   (ScmList         a)   (ScmList         b) = ptrEq a b
scmEqv x@(ScmDottedList _ _) y@(ScmDottedList _ _) = ptrEq x y
scmEqv   (ScmString       a)   (ScmString       b) = ptrEq a b
scmEqv   (ScmFunc _       a)   (ScmFunc _       b) = ptrEq a b
scmEqv   (ScmPrim _       a)   (ScmPrim _       b) = ptrEq a b
scmEqv a b =
   return $ if isNumeric a && isNumeric b
               then isExact a == isExact b && numEq a b
               else False

scmEq a b = if isNumeric a && isNumeric b
               then ptrEq  a b
               else scmEqv a b

scmEqual (ScmList x) (ScmList y) = go x y
 where
   go []     []     = return True
   go []     _      = return False
   go _      []     = return False
   go (a:as) (b:bs) =
      scmEqv a b >>= \e -> if not e then return False else go as bs

scmEqual (ScmString a) (ScmString b) = return$ a == b
scmEqual a b = scmEqv a b

ptrEq :: a -> a -> IO Bool
ptrEq x y = do
   nx <- makeStableName x
   ny <- makeStableName y
   return (nx == ny)

scmEquivalence :: (ScmValue -> ScmValue -> IO Bool) -> String
               -> [ScmValue] -> IO (ErrOr Bool)

scmEquivalence f _ [x,y]   = fmap Right $ f x y
scmEquivalence _ s (_:_:_) = return$ tooManyArgs s
scmEquivalence _ s _       = return$ tooFewArgs  s
