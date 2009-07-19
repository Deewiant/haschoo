-- File created: 2009-07-19 18:34:38

module Haschoo.Evaluator.Primitives (context) where

import Haschoo.Types           ( Datum(..), ScmValue(..)
                               , Context, mkContext, contextSize, scmShowDatum)
import Haschoo.Utils           (ErrOr, compareLength, compareLengths, (.:))
import Haschoo.Evaluator       (eval)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

context :: Context
context = mkContext primitives

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmPrim a b)) $
   [ ("lambda", scmLambda) ]

scmLambda :: [Context] -> [Datum] -> ErrOr ScmValue
scmLambda _   []                          = tooFewArgs "lambda"
scmLambda _   [_]                         = tooFewArgs "lambda"
scmLambda ctx (UnevaledApp params : body) = Right $ ScmFunc name func
 where
   func args =
      case compareLengths args params of
           EQ ->
              case paramNames of
                   Right ns ->
                      let c = subContext ns args
                       in case compareLength params (contextSize c) of
                               EQ ->
                                  -- FIXME: evaluate body in sequence
                                  eval (c : ctx) (last body)
                               LT -> duplicateParam
                               GT -> error "lambda :: the impossible happened"
                   Left bad -> badParam bad
           LT -> tooFewArgs  name
           GT -> tooManyArgs name

   paramNames = mapM f params
    where
      f (UnevaledId x) = Right x
      f x              = Left (scmShowDatum x)

   -- TODO: these should be cached somewhere somehow, not fully recreated every
   -- time: we just need to substitute the parameter values
   name = "<lambda>"
   subContext = mkContext .: zip

   duplicateParam = fail.concat $ ["Duplicate in parameter list of ", name]
   badParam bad   = fail.concat $ ["Invalid parameter '", bad, "' to ", name]

-- FIXME: (lambda x x) is valid, as is dotted-tail notation
scmLambda _ _ = fail "Invalid parameters to lambda"
