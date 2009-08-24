-- File created: 2009-07-19 18:34:38

module Haschoo.Evaluator.Primitives (context) where

import Control.Applicative ((<$>))
import Control.Arrow       (first)
import Control.Monad       (when)
import Control.Monad.Error (throwError)
import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.Array.IArray   (listArray, elems, bounds)
import Data.IORef          ( IORef, readIORef
                           , newIORef, modifyIORef, writeIORef)
import Data.Maybe          (isJust)

import Haschoo.Types            ( Haschoo, runHaschoo, withHaschoo
                                , ScmValue(..), isTrue
                                , Context, mkContext, contextSize
                                , addToContext, contextLookup
                                , listToPair)
import Haschoo.Utils            ( compareLength, compareLengths, (.:)
                                , fromRights, modifyM)
import Haschoo.Evaluator.Eval   (eval, evalBody, maybeEval)
import Haschoo.Evaluator.Macros (mkMacro)
import Haschoo.Evaluator.Utils  (tooFewArgs, tooManyArgs)

import Haschoo.Evaluator.Standard.PairsLists (scmAppend)

context :: Context
context = mkContext primitives

primitives :: [(String, ScmValue)]
primitives = map (\(a,b) -> (a, ScmPrim a b)) $
   [ ("lambda",        scmLambda)
   , ("quote",         scmQuote)
   , ("if",            scmIf)
   , ("set!",          scmSet)
   , ("letrec",        scmLetRec)
   , ("syntax-rules",  scmSyntaxRules)
   , ("let-syntax",    scmLetSyntax)
   , ("letrec-syntax", scmLetRecSyntax)
   , ("quasiquote",    scmQuasiQuote)
   , ("do",            scmDo) ]

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
   e  <- eval expr
   set var e
   return ScmVoid

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

                  ScmVector xs | maxIdx >= 0 ->
                     case elems xs of
                          ScmIdentifier _keyword : pat ->
                             return (ScmVector $ listArray (0, maxIdx-1) pat)

                          _ -> badPattern
                   where
                     maxIdx = snd (bounds xs)

                  _ -> badPattern

      checkPat pat
      return (pat, template, frees template)
    where
      frees (ScmIdentifier i)    = if isLocal i
                                      then []
                                      else [i]
      frees (ScmList       xs)   = concatMap frees xs
      frees (ScmVector     v)    = concatMap frees (elems v)
      frees (ScmDottedList xs x) = frees x ++ concatMap frees xs
      frees _                    = []

      isLocal s = go pattern
       where
         go (ScmIdentifier i)    = s == i
         go (ScmList   xs)       = any go xs
         go (ScmVector v)        = any go (elems v)
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
                    checkEllipsisUse i (length after) template
                    or <$> mapM (go True) ls

                 Nothing -> or <$> mapM (go found) ls
                 _       -> or <$> mapM (go True)  ls

         go _ (ScmDottedList _ (ScmIdentifier "...")) =
            srErr "Ellipsis terminating improper list"

         go _ (ScmDottedList ls _) | isJust (findEllipsis ls) =
            srErr "Ellipsis in improper list"

         go found (ScmDottedList ls x) = do
            found' <- or <$> mapM (go found) ls
            go found' x

         go found (ScmVector v) = go found (ScmList $ elems v)

         go found _ = return found

      checkEllipsisUse var n (ScmList ls) =
            case findEllipsis ls of
                 Just (Just (ScmIdentifier i), after) | var == i ->

                    if (length . filter elliptic . take n $ after) == n
                       then mapM_ (checkEllipsisUse var n) ls
                       else srErr $ "Ellipsis use matches pattern variable " ++
                                    "but not ellipsis count"

                 _ -> mapM_ (checkEllipsisUse var n) ls

      checkEllipsisUse var n (ScmDottedList ls x) = do
         mapM_ (checkEllipsisUse var n) ls
         checkEllipsisUse var n x

      checkEllipsisUse var n (ScmVector v) =
         checkEllipsisUse var n (ScmList $ elems v)

      checkEllipsisUse _ _ _ = return ()

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
   ctx   <- liftIO $ newIORef (mkContext [])
   stack <- fmap (f ctx) get
   mapM_ (go stack ctx) bindings
   withHaschoo (ctx:stack) (evalBody body)
 where
   go stack ctx (ScmList [ScmIdentifier var, binding]) = do
      syntax <- eval binding
      case syntax of
           ScmSyntax pats lits ->
              let macro = mkMacro stack var (map addVar pats) lits
               in liftIO $ modifyIORef ctx (addToContext var macro)
            where
              -- Add the macro's name to the list of free variables to create
              -- the difference between let-syntax and letrec-syntax
              addVar (pat, repl, frees) = (pat, repl, var:frees)

           _ -> badBinding s

   go _ _ _ = badBinding s

   badBinding = throwError . ("Invalid binding to " ++)

syntaxLet _ s (_:_:_) = throwError ("Invalid list of bindings to " ++ s)
syntaxLet _ s _       = tooFewArgs s

scmQuasiQuote :: [ScmValue] -> Haschoo ScmValue
scmQuasiQuote [expr] = finishUnqq <$> unqq 0 expr
 where
   finishUnqq = either (either (uncurry ScmPair) ScmList) id

   finishWrapUnqq s v = Right $ ScmList [ScmIdentifier s, finishUnqq v]

   -- Left results are stuff that need to be spliced
   unqq :: Int
        -> ScmValue
        -> Haschoo (Either (Either (IORef ScmValue, IORef ScmValue) [ScmValue])
                           ScmValue)

   unqq n (ScmList (ScmIdentifier s@"unquote" : vs)) =
      case vs of
           [v] | n == 0    -> Right <$> eval v
               | otherwise -> finishWrapUnqq s <$> unqq (n-1) v
           [] -> tooFewArgs  s
           _  -> tooManyArgs s

   unqq n (ScmList (ScmIdentifier s@"unquote-splicing" : vs)) =
      case vs of
           [v] | n == 0 -> do
              ev <- eval v
              case ev of
                   ScmList evs -> return$ Left . Right $ evs
                   ScmPair a b -> return$ Left . Left  $ (a,b)
                   _           -> notList

               | otherwise -> unqq (n-1) v

           [] -> tooFewArgs  s
           _  -> tooManyArgs s

   unqq n (ScmList (ScmIdentifier s@"quasiquote" : vs)) =
      case vs of
         [v] -> finishWrapUnqq s <$> unqq (n+1) v
         []  -> tooFewArgs  s
         _   -> tooManyArgs s

   unqq n (ScmList xs) =
      Right . either (uncurry ScmPair) ScmList <$>
         (splice =<< mapM (unqq n) xs)

   unqq n (ScmDottedList xs x) = do
      xs' <- splice =<< mapM (unqq n) xs
      x'  <- unqq n x
      case x' of
           Left _  -> throwError "quasiquote :: unexpected unquote-splicing"
           Right v ->
              case xs' of
                   Left (a,b) -> Right <$> (liftIO $ snocPair (ScmPair a b) v)
                   Right ys   -> return$ Right $ ScmDottedList ys v

   unqq _ x = return (Right x)

   splice :: [Either (Either (IORef ScmValue, IORef ScmValue) [ScmValue])
                     ScmValue]
          -> Haschoo (Either (IORef ScmValue, IORef ScmValue) [ScmValue])
   splice xs =
      case fromRights xs of
           Just xs' -> return$ Right xs'
           Nothing  ->
              let listified = map (either id (Right . (:[]))) xs
               in case fromRights listified of
                       Just xs' -> return$ Right (concat xs')
                       Nothing  -> do
                          cat <- liftIO $
                             scmAppend =<<
                                mapM (either (return . uncurry ScmPair)
                                             (fmap fst . listToPair))
                                     listified
                          case cat of
                               Left  _             -> notList
                               Right (ScmPair a b) -> return$ Left (a,b)
                               Right _             ->
                                  error "splice :: the impossible happened"

   snocPair :: ScmValue -> ScmValue -> IO ScmValue
   snocPair p@(ScmPair _ b) x = do
      b' <- liftIO $ readIORef b
      case b' of
           ScmList xs      -> do liftIO $ writeIORef b (ScmList (xs ++ [x]))
                                 return p
           q@(ScmPair _ _) -> snocPair q x

           -- splice's return value should be a proper list
           _               -> error "snocPair :: the impossible happened"

   snocPair _ _ = error "snocPair :: the impossible happened"

   notList :: Haschoo a
   notList = throwError "quasiquote :: unquote-splicing did not return a list"

scmQuasiQuote [] = tooFewArgs  "quasiquote"
scmQuasiQuote _  = tooManyArgs "quasiquote"

scmDo :: [ScmValue] -> Haschoo ScmValue
scmDo (ScmList vars@(_:_) : ScmList (test:result) : cmds) = do
   inited <- mapM initVar vars
   c' <- liftIO $ newIORef (mkContext $ map fst inited)
   ctx <- get
   withHaschoo (c':ctx) (go $ map (first fst) inited)
 where
   go :: [(String, ScmValue)] -> Haschoo ScmValue
   go varSteps = do
      stop <- eval test
      if isTrue stop
         then if null result then return ScmVoid else last <$> mapM eval result
         else do
            mapM_ eval cmds
            evalSteps <- mapM (eval.snd) varSteps
            mapM_ (uncurry set) $ zip (map fst varSteps) evalSteps
            go varSteps

   initVar :: ScmValue -> Haschoo ((String, ScmValue), ScmValue)
   initVar (ScmList (var@(ScmIdentifier v) : i : step)) = do
      step' <- case step of
                    []  -> return var
                    [s] -> return s
                    _   -> throwError "do :: more than one step expression?"
      ei <- eval i
      return ((v, ei), step')

   initVar _ = throwError "do :: expected list of variables and initializers"

scmDo (_:_:_:_) = tooManyArgs "do"
scmDo (_:_:_)   = throwError  "do :: empty lists or nonlists given"
scmDo _         = tooFewArgs  "do"

set :: String -> ScmValue -> Haschoo ()
set var evaled = modifyM f
 where
   f (c:cs) = do
      c' <- liftIO $ readIORef c
      case contextLookup var c' of
           Just _  -> do
              liftIO $ modifyIORef c (addToContext var evaled)
              return (c:cs)

           Nothing -> (c:) <$> f cs

   f [] = throwError $ "Unbound identifier '" ++ var ++ "'"
