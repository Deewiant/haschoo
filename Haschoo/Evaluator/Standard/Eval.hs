-- File created: 2009-08-08 16:39:10

module Haschoo.Evaluator.Standard.Eval (procedures) where

import Data.IORef (newIORef)

import Haschoo.Types           (ScmValue(..), runHaschoo)
import Haschoo.Utils           (ErrOr)
import Haschoo.Evaluator.Eval  (eval)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b))
   [ ("eval", scmEval)

   -- scheme-report-environment and null-environment are in Haschoo.Stdlib to
   -- avoid circular dependencies
   ]

scmEval :: [ScmValue] -> IO (ErrOr ScmValue)
scmEval [v, ScmContext ctx] = mapM newIORef ctx >>= flip runHaschoo (eval v)
scmEval [_,_]               = return$ Left "Nonenvironmental argument to eval"
scmEval (_:_:_)             = return$ tooManyArgs "eval"
scmEval _                   = return$ tooFewArgs  "eval"
