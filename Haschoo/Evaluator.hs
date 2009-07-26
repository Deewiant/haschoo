-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator (evalToplevel) where

import Control.Monad.Error (throwError)
import Control.Monad.State (get)
import Data.IORef          (newIORef)

import Haschoo.Types            ( Haschoo, runHaschoo
                                , ScmValue( ScmList, ScmIdentifier, ScmSyntax
                                          , ScmVoid)
                                , Context)
import Haschoo.Utils            (ErrOr)
import Haschoo.Evaluator.Eval   (eval, evalBody, define)
import Haschoo.Evaluator.Macros (mkMacro)
import Haschoo.Evaluator.Utils  (tooFewArgs, tooManyArgs)

import qualified Haschoo.Evaluator.Standard   as Standard
import qualified Haschoo.Evaluator.Primitives as Primitives

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

evalToplevel :: [ScmValue] -> IO (ErrOr ScmValue)
evalToplevel prog = do
   ctx <- mapM newIORef toplevelContext
   runHaschoo ctx (fmap last $ mapM f prog)
 where
   f (ScmList (ScmIdentifier "define-syntax":xs)) = do
      scmDefineSyntax xs
      return ScmVoid
   f d = evalBody [d]

toplevelContext :: [Context]
toplevelContext = [Standard.context, Primitives.context]

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
