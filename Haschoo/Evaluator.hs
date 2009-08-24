-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator (evalToplevel) where

import Control.Monad.Error (throwError)
import Control.Monad.State (get)
import Data.IORef          (IORef)

import Haschoo.Types            ( Haschoo, runHaschoo
                                , ScmValue( ScmList, ScmIdentifier, ScmSyntax
                                          , ScmVoid)
                                , Context)
import Haschoo.Utils            (ErrOr)
import Haschoo.Evaluator.Eval   (eval, evalBody, define)
import Haschoo.Evaluator.Macros (mkMacro)
import Haschoo.Evaluator.Utils  (tooFewArgs, tooManyArgs)

-- Programs consist of three things:
--    expressions        - valid anywhere
--    definitions        - valid at toplevel or at the beginning of a <body>
--    syntax definitions - valid at toplevel
--
-- Where <body> is a body of one of:
--    lambda, let, let*, letrec, let-syntax, letrec-syntax,
--    or procedure definition
--
-- Hence we need three eval functions (the following, evalBody, and eval) and
-- hence definitions are handled separately in the former two instead of being
-- ordinary primitives.
--
-- "begin" needs special treatment as well, since it depends on the evaluation
-- level (R5RS 5.1, 5.2.2). We special-case it here and in evalBody, leaving
-- ordinary expression usage to the definition given in R5RS 7.3.

evalToplevel :: [IORef Context] -> [ScmValue] -> IO (ErrOr ScmValue)
evalToplevel ctx prog = runHaschoo ctx (fmap last $ mapM f prog)
 where
   f (ScmList (ScmIdentifier i : xs))
      | i == "define-syntax" = do
         scmDefineSyntax xs
         return ScmVoid

      | i == "begin" = fmap last $ mapM f xs

   f d = evalBody [d]

scmDefineSyntax :: [ScmValue] -> Haschoo ()
scmDefineSyntax [ScmIdentifier var, x] = do
   x' <- eval x
   case x' of
        ScmSyntax pats lits -> do
           ctx <- get
           define var (mkMacro ctx var pats lits)

        _ -> throwError "define-syntax :: expected instance of syntax-rules"

scmDefineSyntax [_,_]   = throwError  "define-syntax :: expected identifier"
scmDefineSyntax (_:_:_) = tooManyArgs "define-syntax"
scmDefineSyntax _       = tooFewArgs  "define-syntax"
