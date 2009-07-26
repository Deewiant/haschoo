-- File created: 2009-07-26 16:26:13

module Haschoo.Evaluator.Eval (eval, maybeEval, evalBody, define) where

import Control.Monad       (msum)
import Control.Monad.Error (throwError, catchError)
import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.IORef          (IORef, readIORef, modifyIORef)

import qualified Data.IntMap as IM
import qualified Data.ListTrie.Patricia.Map.Enum as TM
import           Data.ListTrie.Patricia.Map.Enum (TrieMap)

import Haschoo.Types           ( Haschoo
                               , ScmValue( ScmPrim, ScmFunc, ScmMacro
                                         , ScmList, ScmDottedList
                                         , ScmVoid, ScmIdentifier)
                               , MacroCall(..)
                               , Context, valMap, addToContext, contextLookup)
import Haschoo.Utils           (lazyMapM, modifyM)
import Haschoo.Evaluator.Utils (tooFewArgs)

eval :: ScmValue -> Haschoo ScmValue
eval = evalWithSpecialCases TM.empty

evalWithSpecialCases :: TrieMap Char [IORef Context]
                     -> ScmValue -> Haschoo ScmValue
evalWithSpecialCases specials (ScmIdentifier s) =
   case TM.lookup s specials of
        Just specialCtx -> lookupId specialCtx
        Nothing         -> lookupId =<< get
 where
   lookupId ctx = do
      lookups <- lazyMapM (fmap (contextLookup s) . readIORef) ctx
      case msum lookups of
           Nothing    -> throwError $ "Unbound identifier '" ++ s ++ "'"
           Just (c,i) -> case IM.lookup i (valMap c) of
                              Just v  -> return v
                              Nothing ->
                                 throwError $ "Internal error looking up " ++ s

evalWithSpecialCases _  (ScmList [])     = throwError "Empty application"
evalWithSpecialCases sp (ScmList (x:xs)) = do
   evaledHead <- evalWithSpecialCases sp x
   case evaledHead of
        ScmPrim _ f -> f xs
        ScmFunc _ f -> do
           args   <- mapM (evalWithSpecialCases sp) xs
           result <- liftIO $ f args
           case result of
                Left  s   -> throwError s
                Right val -> return val

        m@(ScmMacro _ _) -> evalMacro sp m (MCList xs)

        _ -> throwError "Can't apply non-function"

evalWithSpecialCases sp (ScmDottedList (x:xs) y) = do
   evaledHead <- evalWithSpecialCases sp x
   case evaledHead of
        m@(ScmMacro _ _) -> evalMacro sp m (MCDotted xs y)

        _ -> throwError "Ill-formed procedure application: no dots allowed"

evalWithSpecialCases _ v = return v

evalMacro :: TrieMap Char [IORef Context]
          -> ScmValue -> MacroCall -> Haschoo ScmValue
evalMacro sp (ScmMacro _ f) xs = do
   (expansion, frees) <- f xs
   evalWithSpecialCases (TM.union frees sp) expansion

evalMacro _ _ _ = error "evalMacro :: the impossible happened: not a macro"

maybeEval :: ScmValue -> Haschoo (Maybe ScmValue)
maybeEval = (`catchError` const (return Nothing)) . fmap Just . eval

evalBody :: [ScmValue] -> Haschoo ScmValue
evalBody (ScmList (ScmIdentifier "define":xs) : ds) =
   scmDefine xs >> evalBody ds

evalBody ds@(_:_) = fmap last . mapM eval $ ds
evalBody []       = return ScmVoid

define :: String -> ScmValue -> Haschoo ()
define var body = eval body >>= modifyM . f
 where
   f e (c:cs) = do
      liftIO $ modifyIORef c (addToContext var e)
      return (c:cs)
   f _ []     = error "define :: the impossible happened: empty context stack"

scmDefine :: [ScmValue] -> Haschoo ()
scmDefine [ScmIdentifier var, expr] = define var expr

scmDefine (ScmList (ScmIdentifier func : params) : body) =
   define func . ScmList $ ScmIdentifier "lambda" : ScmList params : body

scmDefine (ScmDottedList (ScmIdentifier func : params) final : body) =
   define func . ScmList $ ScmIdentifier "lambda"
                           : (if null params
                                 then final
                                 else ScmDottedList params final)
                           : body

scmDefine (_:_:_) = throwError "define :: expected identifier"
scmDefine _       = tooFewArgs "define"
