-- File created: 2009-07-19 18:34:38

module Haschoo.Evaluator.Primitives (context) where

import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)

import Haschoo.Types           ( Haschoo, withHaschoo
                               , Datum(..), ScmValue(..), isTrue
                               , Context, mkContext, contextSize
                               , addToContext, contextLookup
                               , scmShowDatum)
import Haschoo.Utils           (ErrOr, compareLength, compareLengths, (.:))
import Haschoo.Evaluator       (eval, evalBody)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

context :: Context
context = mkContext primitives

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmPrim a b)) $
   [ ("lambda", scmLambda)
   , ("quote",  scmQuote)
   , ("if",     scmIf)
   , ("set!",   scmSet) ]

scmLambda :: [Datum] -> Haschoo ScmValue
scmLambda []                          = tooFewArgs "lambda"
scmLambda [_]                         = tooFewArgs "lambda"
scmLambda (UnevaledApp params : body) = return $ ScmPrim name func
 where
   func xs = do
      args <- mapM eval xs
      case compareLengths args params of
           EQ ->
              case paramNames of
                   Right ns -> do
                      let c = subContext ns args
                      case compareLength params (contextSize c) of
                           EQ -> withHaschoo (c:) $ evalBody body
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

   duplicateParam =
      throwError.concat $ ["Duplicate in parameter list of ", name]
   badParam bad   =
      throwError.concat $ ["Invalid parameter '", bad, "' to ", name]

-- FIXME: (lambda x x) is valid, as is dotted-tail notation
scmLambda _ = throwError "Invalid parameters to lambda"

scmQuote :: [Datum] -> Haschoo ScmValue
scmQuote [x] = return $ ScmQuoted x
scmQuote []  = tooFewArgs  "quote"
scmQuote _   = tooManyArgs "quote"

scmIf :: [Datum] -> Haschoo ScmValue
scmIf [b,x,y] = eval b >>= \t -> eval $ if isTrue t then x else y
scmIf [b,x]   = eval b >>= \t -> if isTrue t then eval x else return ScmVoid
scmIf (_:_:_) = tooManyArgs "if"
scmIf _       = tooFewArgs "if"

scmSet :: [Datum] -> Haschoo ScmValue
scmSet [UnevaledId var, expr] = do
   e    <- eval expr
   ctx  <- get
   ctx' <- f e ctx
   put ctx'
   return ScmVoid
 where
   f e (c:cs) = case contextLookup var c of
                     Just _  -> return $ addToContext var e c : cs
                     Nothing -> fmap (c:) (f e cs)

   f e []     = throwError $ "Unbound identifier '" ++ var ++ "'"
