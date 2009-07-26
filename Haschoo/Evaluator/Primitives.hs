-- File created: 2009-07-19 18:34:38

module Haschoo.Evaluator.Primitives (context) where

import Control.Applicative ((<$>))
import Control.Monad       (when)
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Control.Monad.Trans (liftIO)
import Data.IORef          (IORef, newIORef, readIORef, modifyIORef)
import Data.Maybe          (isJust)

import Haschoo.Types            ( Haschoo, runHaschoo, withHaschoo
                                , ScmValue(..), isTrue
                                , Context, mkContext, contextSize
                                , addToContext, contextLookup)
import Haschoo.Utils            (compareLength, compareLengths, (.:))
import Haschoo.Evaluator.Eval   (eval, evalBody, maybeEval)
import Haschoo.Evaluator.Macros (mkMacro)
import Haschoo.Evaluator.Utils  (tooFewArgs, tooManyArgs)

context :: Context
context = mkContext primitives

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmPrim a b)) $
   [ ("lambda",         scmLambda)
   , ("quote",          scmQuote)
   , ("if",             scmIf)
   , ("set!",           scmSet)
   , ("letrec",         scmLetRec)
   , ("syntax-rules",   scmSyntaxRules)
   , ("let-syntax",     scmLetSyntax)
   , ("letrec-syntax", scmLetRecSyntax) ]

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
scmLetRec (ScmList l : b@(_:_)) = doLetRec l b
scmLetRec (_:_:_) = throwError "Invalid list of bindings to letrec"
scmLetRec _       = tooFewArgs "letrec"

doLetRec :: [ScmValue] -> [ScmValue] -> Haschoo ScmValue
doLetRec bindings body = do
   ctx      <- liftIO $ newIORef (mkContext [])
   newStack <- fmap (ctx:) get
   mapM_ (bind newStack ctx) bindings
   withHaschoo newStack (evalBody body)
 where
   bind ctxStack ctx (ScmList [ScmIdentifier var, val]) = do
      evaled <- withHaschoo ctxStack (eval val)
      liftIO $ modifyIORef ctx (addToContext var evaled)

   bind _  _ _ = throwError "Invalid binding to letrec"

scmSyntaxRules :: [ScmValue] -> Haschoo ScmValue
scmSyntaxRules (ScmList lits : rest) = do
   evaledLits <- mapM unlit lits
   pats       <- mapM unpat rest
   return$ ScmSyntax pats evaledLits
 where
   unpat (ScmList [pattern, template]) = do
      pat <- case pattern of
                  -- It really does seem to me that the keyword in a syntax
                  -- rule is ignored according to R5RS 4.3.2: what names the
                  -- macro is the identifier in define-syntax / let-syntax /
                  -- letrec-syntax, the name in the pattern match is
                  -- irrelevant.
                  ScmList (ScmIdentifier _keyword : pat) ->
                     return (ScmList pat)
                  ScmDottedList (ScmIdentifier _keyword : pat) pat' ->
                     return (ScmDottedList pat pat')
                  -- TODO: vector
                  _ -> badPattern

      checkPat pat
      return (pat, template, frees template)
    where
      frees (ScmIdentifier i)    = if isLocal i
                                      then []
                                      else [i]
      frees (ScmList       xs)   = concatMap frees xs
      frees (ScmDottedList xs x) = frees x ++ concatMap frees xs
      frees _                    = []

      isLocal s = go pattern
       where
         go (ScmIdentifier i)    = s == i
         go (ScmList xs)         = any go xs
         go (ScmDottedList xs x) = any go xs || go x
         go _                    = False

      checkPat p = do
         found <- go False p
         when (not found) (assertNoEllipses template)
       where
         go found (ScmList ls) =
            case findEllipsis ls of
                 Just (_, after)
                    | not (all elliptic after) ->
                       srErr
                          "Nonelliptic identifier follows ellipsis in pattern"

                 Just (Nothing, _) ->
                    srErr "Ellipsis not preceded by anything in pattern"

                 Just (Just (ScmIdentifier i), after) -> do
                    used <- checkEllipsisUse i (length after) template
                    when (not used) $
                       srErr . concat $
                          [ "No matching ellipsis use for pattern variable '"
                          , i, "'"]

                    or <$> mapM (go True) ls

                 Nothing -> or <$> mapM (go found) ls
                 _       -> or <$> mapM (go True)  ls

         go _ (ScmDottedList _ (ScmIdentifier "...")) =
            srErr "Ellipsis terminating improper list"

         go found (ScmDottedList ls x) = do
            found' <- or <$> mapM (go found) ls
            go found' x

         -- TODO: vector

         go found _ = return found

      checkEllipsisUse = go False
       where
         go found var n (ScmList ls) =
            case findEllipsis ls of
                 Just (Just (ScmIdentifier i), after) | var == i ->

                    if all elliptic (take n after)
                       then or <$> mapM (go True var n) ls
                       else srErr $ "Ellipsis use matches pattern variable " ++
                                    "but not ellipsis count"

                 _ -> or <$> mapM (go found var n) ls

         go found var n (ScmDottedList ls x) = do
            found' <- or <$> mapM (go found var n) ls
            go found' var n x

         -- TODO: vector

         go found _ _ _ = return found

      assertNoEllipses (ScmList ls) =
         if isJust (findEllipsis ls)
            then srErr "Ellipsis used as identifier in template"
            else mapM_ assertNoEllipses ls

      assertNoEllipses (ScmDottedList ls x) =
         mapM_ assertNoEllipses ls >> assertNoEllipses x

      assertNoEllipses _ = return ()

      findEllipsis []                             = Nothing
      findEllipsis (    ScmIdentifier "..." : xs) = Just (Nothing, xs)
      findEllipsis (x : ScmIdentifier "..." : xs) = Just (Just x,  xs)
      findEllipsis (_:xs)                         = findEllipsis xs

   unpat _ = badPattern

   badPattern = throwError "Invalid pattern in syntax-rules"

   unlit lit@(ScmIdentifier i) =
      if elliptic lit
         then throwError "Elliptic literal to syntax-rules"
         else fmap ((,) i) (maybeEval lit)

   unlit _ = throwError "Nonidentifier literal to syntax-rules"

   elliptic (ScmIdentifier "...") = True
   elliptic _                     = False

   srErr = throwError . (++ " in syntax-rules")

scmSyntaxRules [] = tooFewArgs "syntax-rules"
scmSyntaxRules _  = throwError "Invalid list of literals to syntax-rules"

scmLetSyntax, scmLetRecSyntax :: [ScmValue] -> Haschoo ScmValue
scmLetSyntax    = syntaxLet (flip const) "let-syntax"
scmLetRecSyntax = syntaxLet (:)          "letrec-syntax"

syntaxLet :: (IORef Context -> [IORef Context] -> [IORef Context]) -> String
          -> [ScmValue] -> Haschoo ScmValue

syntaxLet f s (ScmList bindings : body@(_:_)) = do
   ctx    <- liftIO $ newIORef (mkContext [])
   stack  <- fmap (f ctx) get
   mapM_ (go stack ctx) bindings
   withHaschoo (ctx:stack) (evalBody body)
 where
   go stack ctx (ScmList [ScmIdentifier var, binding]) = do
      syntax <- eval binding
      case syntax of
           ScmSyntax pats lits ->
              liftIO $
                 modifyIORef ctx
                    (addToContext var $ mkMacro stack var pats lits)

           _ -> badBinding s

   go _ _ _ = badBinding s

   badBinding = throwError . ("Invalid binding to " ++)

syntaxLet _ s (_:_:_) = throwError ("Invalid list of bindings to " ++ s)
syntaxLet _ s _       = tooFewArgs s
