-- File created: 2009-07-19 18:34:38

module Haschoo.Evaluator.Primitives (context) where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Control.Monad.Trans (liftIO)
import Data.IORef          (IORef, newIORef, readIORef, modifyIORef)

import Haschoo.Types           ( Haschoo, withHaschoo
                               , Datum(..), ScmValue(..), isTrue
                               , Context, mkContext, contextSize
                               , addToContext, contextLookup
                               , scmShowDatum)
import Haschoo.Utils           (compareLength, compareLengths, (.:))
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
scmLambda []                                     = tooFewArgs "lambda"
scmLambda [_]                                    = tooFewArgs "lambda"
scmLambda (UnevaledApp ps                : body) = mkΛ ps  Nothing body <$> get
scmLambda (                UnevaledId t  : body) = mkΛ [] (Just t) body <$> get
scmLambda (DottedList  ps (UnevaledId t) : body) = mkΛ ps (Just t) body <$> get

scmLambda _ = throwError "Invalid parameters to lambda"

mkΛ :: [Datum] -> Maybe String -> [Datum] -> [IORef Context] -> ScmValue
mkΛ formals tailParams body ctx = ScmPrim name func
 where
   func = \xs -> do
      case compareLengths xs formals of
           (LT,_)      -> tooFewArgs name
           (order,len) -> do
              args <- mapM eval xs
              case tailParams of
                   Nothing ->
                      if order == EQ
                         then make id args
                         else tooManyArgs name
                   Just n ->
                      let (normal, tailArgs) = splitAt len args
                       in make (++[n]) (normal ++ [ScmList tailArgs])
    where
      make mkParams args =
         case paramNames of
              Right ns ->
                 let ns' = mkParams ns
                     c   = subContext ns' args
                  in case compareLength ns' (contextSize c) of
                          EQ -> do
                             c' <- liftIO $ newIORef c
                             withHaschoo (const (c':ctx)) $ evalBody body

                          LT -> duplicateParam
                          GT -> error "lambda :: the impossible happened"

              Left bad -> badParam bad

      paramNames = mapM f formals
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
   f :: ScmValue -> [IORef Context] -> Haschoo [IORef Context]
   f e (c:cs) = do
      c' <- liftIO $ readIORef c
      case contextLookup var c' of
           Just _  -> do
              liftIO $ modifyIORef c (addToContext var e)
              return (c:cs)

           Nothing -> fmap (c:) (f e cs)

   f _ [] = throwError $ "Unbound identifier '" ++ var ++ "'"

scmSet [_,_]   = throwError $ "Non-identifier argument to set!"
scmSet (_:_:_) = tooManyArgs "set!"
scmSet _       = tooFewArgs  "set!"
