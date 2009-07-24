-- File created: 2009-07-19 18:34:38

module Haschoo.Evaluator.Primitives (context) where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Control.Monad.Trans (liftIO)
import Data.IORef          (IORef, newIORef, readIORef, modifyIORef)

import Haschoo.Types           ( Haschoo, runHaschoo
                               , ScmValue(..), isTrue
                               , Context, mkContext, contextSize
                               , addToContext, contextLookup)
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
   , ("set!",   scmSet)
   , ("letrec", scmLetRec) ]

scmLambda :: [ScmValue] -> Haschoo ScmValue
scmLambda []                                          = tooFewArgs "lambda"
scmLambda [_]                                         = tooFewArgs "lambda"
scmLambda (ScmList       ps                   : body) = mkΛ ps  Nothing body
scmLambda (                  ScmIdentifier t  : body) = mkΛ [] (Just t) body
scmLambda (ScmDottedList ps (ScmIdentifier t) : body) = mkΛ ps (Just t) body

scmLambda _ = throwError "Invalid parameters to lambda"

mkΛ :: [ScmValue] -> Maybe String -> [ScmValue] -> Haschoo ScmValue
mkΛ formals tailParams body = ScmFunc name . func <$> get
 where
   func ctx = \args -> do
      case compareLengths args formals of
           (LT,_)      -> return$ tooFewArgs name
           (order,len) -> do
              case tailParams of
                   Nothing ->
                      if order == EQ
                         then make id args
                         else return$ tooManyArgs name
                   Just n ->
                      let (normal, tailArgs) = splitAt len args
                       in make (++[n]) (normal ++ [ScmList tailArgs])
    where
      make mkParams args =
         case paramNames of
              Just ns ->
                 let ns' = mkParams ns
                     c   = subContext ns' args
                  in case compareLength ns' (contextSize c) of
                          EQ -> do
                             c' <- newIORef c
                             runHaschoo (c':ctx) $ evalBody body

                          LT -> return duplicateParam
                          GT -> error "lambda :: the impossible happened"

              Nothing -> return badParam

      paramNames = mapM f formals
       where
         f (ScmIdentifier x) = Just x
         f _                 = Nothing

   -- TODO: these should be cached somewhere somehow, not fully recreated every
   -- time: we just need to substitute the parameter values
   name = "<lambda>"
   subContext = mkContext .: zip

   duplicateParam =
      throwError.concat $ ["Duplicate in parameter list of ", name]
   badParam      =
      throwError.concat $ ["Invalid identifier in parameter list of ", name]

scmQuote :: [ScmValue] -> Haschoo ScmValue
scmQuote [x] = return x
scmQuote []  = tooFewArgs  "quote"
scmQuote _   = tooManyArgs "quote"

scmIf :: [ScmValue] -> Haschoo ScmValue
scmIf [b,x,y] = eval b >>= \t -> eval $ if isTrue t then x else y
scmIf [b,x]   = eval b >>= \t -> if isTrue t then eval x else return ScmVoid
scmIf (_:_:_) = tooManyArgs "if"
scmIf _       = tooFewArgs "if"

scmSet :: [ScmValue] -> Haschoo ScmValue
scmSet [ScmIdentifier var, expr] = do
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

scmLetRec :: [ScmValue] -> Haschoo ScmValue
scmLetRec (ScmList l : b) = doLetRec l b
scmLetRec [_]             = tooFewArgs "letrec"
scmLetRec []              = tooFewArgs "letrec"
scmLetRec _               = throwError $ "Invalid list of bindings to letrec"

doLetRec :: [ScmValue] -> [ScmValue] -> Haschoo ScmValue
doLetRec bindings body = do
   ctxStack <- get
   result   <- liftIO $ do
                  ctx <- newIORef (mkContext [])
                  let newStack = ctx:ctxStack
                  err <- bind bindings newStack ctx
                  case err of
                       Just s  -> return (Left s)
                       Nothing ->
                          runHaschoo newStack $ evalBody body

   case result of
        Left  s -> throwError s
        Right v -> return v
 where
   bind (ScmList [ScmIdentifier var, val] : bs) ctxStack ctx = do
      val' <- runHaschoo ctxStack $ eval val
      case val' of
           Left  err -> return (Just err)
           Right res -> do
              modifyIORef ctx (addToContext var res)
              bind bs ctxStack ctx

   bind [] _ _ = return Nothing
   bind _  _ _ = return (Just "Invalid binding to letrec")
