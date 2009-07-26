-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator (evalToplevel) where


import Haschoo.Types            (Haschoo, ScmValue)
import Haschoo.Evaluator.Eval   (evalBody)

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

-- TODO: syntax definitions
evalToplevel :: [ScmValue] -> Haschoo ScmValue
evalToplevel = fmap last . mapM (evalBody . return)
