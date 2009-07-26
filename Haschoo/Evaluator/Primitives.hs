-- File created: 2009-07-19 18:34:38

{-# LANGUAGE PatternGuards #-}

module Haschoo.Evaluator.Primitives (context) where

import Control.Applicative         ((<$>))
import Control.Monad.Error         (throwError, catchError)
import Control.Monad.Loops         (andM, allM, firstM)
import Control.Monad.State         (get, put)
import Control.Monad.Trans         (lift, liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.IORef                  (IORef, newIORef, readIORef, modifyIORef)
import Data.List                   (find)
import Data.Maybe                  (isNothing)

import qualified Data.ListTrie.Patricia.Map.Enum as TM
import Data.ListTrie.Patricia.Map.Enum (TrieMap)

import Haschoo.Types           ( Haschoo, runHaschoo, withHaschoo
                               , ScmValue(..), MacroCall(..), isTrue
                               , Context, mkContext, contextSize
                               , addToContext, contextLookup)
import Haschoo.Utils           ( compareLength, compareLengths, (.:)
                               , initLast2Maybe, eqWithM, ptrEq)
import Haschoo.Evaluator       (eval, evalBody)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

import Haschoo.Evaluator.Standard.Equivalence (scmEqual)

context :: Context
context = mkContext primitives

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmPrim a b)) $
   [ ("lambda",       scmLambda)
   , ("quote",        scmQuote)
   , ("if",           scmIf)
   , ("set!",         scmSet)
   , ("letrec",       scmLetRec)
   , ("syntax-rules", scmSyntaxRules)
   , ("let-syntax",   scmLetSyntax) ]

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
scmSyntaxRules (ScmList ls : rest) = do
   lits <- mapM unlit ls
   pats <- mapM unpat rest
   return$ ScmSyntax pats lits
 where
   unpat (ScmList [pattern, template]) =
      case pattern of
           -- It really does seem to me that the keyword in a syntax rule is
           -- ignored according to R5RS 4.3.2: what names the macro is the
           -- identifier in define-syntax / let-syntax / letrec-syntax, the
           -- name in the pattern match is irrelevant.
           ScmList (ScmIdentifier _keyword : pat) ->
              return (ScmList pat, template)

           ScmDottedList (ScmIdentifier _keyword : pat) pat' ->
              return (ScmDottedList pat pat', template)

           -- TODO: vector

           _ -> badPattern

   unpat _ = badPattern

   badPattern = throwError "Invalid pattern in syntax-rules"

   unlit lit@(ScmIdentifier i) = fmap ((,) i) (maybeEval lit)

   unlit _ = throwError "Nonidentifier literal to syntax-rules"

scmSyntaxRules [] = tooFewArgs "syntax-rules"
scmSyntaxRules _  = throwError "Invalid list of literals to syntax-rules"

scmLetSyntax :: [ScmValue] -> Haschoo ScmValue
scmLetSyntax (ScmList bindings : body@(_:_)) = do
   stack  <- get
   ctx    <- liftIO $ newIORef (mkContext [])
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

           _ -> throwError "Invalid binding to let-syntax"

   go _ _ _ = throwError "Invalid binding to let-syntax"

scmLetSyntax (_:_:_) = throwError "Invalid list of bindings to let-syntax"
scmLetSyntax _       = tooFewArgs "let-syntax"

mkMacro :: [IORef Context]
        -> String
        -> [(ScmValue, ScmValue)]
        -> [(String, Maybe ScmValue)]
        -> ScmValue
mkMacro ctxStack name pats lits = ScmMacro name ctxStack f
 where
   f args = do
      (matching, replacements) <- runWriterT $ firstM (match args . fst) pats
      case matching of
           Nothing    -> throwError ("Invalid use of macro " ++ name)
           Just (_,v) -> return$ replaceVars replacements v

   match :: MacroCall -> ScmValue
         -> WriterT (TrieMap Char ScmValue) Haschoo Bool

   -- Turning MCDotted to ScmDottedList here means that the list in the
   -- ScmDottedList may actually be empty, which can't happen normally
   match (MCList   xs)   = match1 (ScmList xs)
   match (MCDotted xs x) = match1 (ScmDottedList xs x)

   -- Paraphrasing R5RS 4.3.2:
   --
   -- "... formally, an input form F matches a pattern P iff:" ...
   match1 :: ScmValue -> ScmValue
          -> WriterT (TrieMap Char ScmValue) Haschoo Bool

   match1 arg (ScmIdentifier i) =
      case find ((== i).fst) lits of
           -- ... "P is a non-literal identifier" ...
           Nothing           -> tell (TM.singleton i arg) >> return True
           Just (_, binding) ->
              case arg of
                   -- ... "P is a literal identifier and F is an identifier
                   -- with the same binding" ...
                   x@(ScmIdentifier _) -> do
                      xb <- lift $ maybeEval x
                      case binding of
                           Nothing -> return (isNothing xb)
                           Just pb ->
                              maybe (return False) (liftIO . ptrEq pb) xb

                   _  -> return False

   match1 (ScmList args) (ScmList ps) =
      case initLast2Maybe ps of
           -- ... "P is of the form (P1 ... Pn Pm <ellipsis>) where <ellipsis>
           -- is the identifier ... and F is a list of at least n forms, the
           -- first n of which match P1 through Pn and each remaining element
           -- matches Pm" ...
           Just (ps', p, ScmIdentifier "...") ->
              let (xs, ys) = splitAt (length ps') args
               in andM [allMatch xs ps', allM (\a -> match1 a p) ys]

           -- ... "P is a list (P1 ... Pn) and F is a list of n forms that
           -- match P1 through Pn" ...
           _ -> allMatch args ps

   -- ... "P is an improper list (P1 ... Pn . Pm)" ...
   match1 args (ScmDottedList ps p) =
      case args of
           -- ... "and F is a list or improper list of n or more forms that
           -- match P1 through Pn and whose nth cdr matches Pm" ...
           ScmList       as -> let (xs, ys) = splitAt (length ps) as
                                in andM [allMatch xs ps, match (MCList ys) p]
           ScmDottedList as a ->   andM [allMatch as ps, match1 a p]
           _                  -> return False

   -- TODO: vectors

   -- ... "P is a datum and F is equal to P in the sense of the equal?
   -- procedure".
   match1 arg p = liftIO $ scmEqual arg p

   allMatch = eqWithM match1

   replaceVars replacements v@(ScmIdentifier i)
      | Just r <- TM.lookup i replacements = r
      | otherwise                          = v

   replaceVars rs (ScmList       l)   = ScmList       (map (replaceVars rs) l)
   replaceVars rs (ScmDottedList l x) = ScmDottedList (map (replaceVars rs) l)
                                                      (replaceVars rs x)

   replaceVars _ v = v

--- Utils

maybeEval :: ScmValue -> Haschoo (Maybe ScmValue)
maybeEval = (`catchError` const (return Nothing)) . fmap Just . eval
