-- File created: 2009-07-22 21:05:24

module Haschoo.Evaluator.Standard.Control (procedures) where

import Haschoo.Types           (ScmValue(..), pairToList)
import Haschoo.Utils           (ErrOr, initLast)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs, notList)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("procedure?",  return . fmap ScmBool . scmIsProcedure)
   , ("apply",       scmApply)
   ]

scmIsProcedure :: [ScmValue] -> ErrOr Bool
scmIsProcedure [ScmFunc _ _] = Right True
scmIsProcedure [_]           = Right False
scmIsProcedure []            = tooFewArgs  "procedure?"
scmIsProcedure _             = tooManyArgs "procedure?"

scmApply :: [ScmValue] -> IO (ErrOr ScmValue)
scmApply (ScmFunc _ f : args@(_:_)) =
   case initLast args of
        (xs, ScmList ys)       -> f (xs ++ ys)
        (xs, ys@(ScmPair _ _)) -> do
           ys' <- pairToList ys
           case ys' of
                Right (ScmList l) -> f (xs ++ l)
                _                 -> return$ notList "apply"
        (_, _)                 -> return$ notList "apply"

scmApply (_:_) = return$ Left "Nonprocedural argument to apply"
scmApply _     = return$ tooFewArgs "apply"
