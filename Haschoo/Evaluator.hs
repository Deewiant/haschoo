-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator where

import           Control.Monad       (liftM2, msum)
import           Control.Monad.Error (throwError)
import           Control.Monad.State (get)
import           Control.Monad.Trans (liftIO)
import qualified Data.IntMap as IM
import qualified Data.ListTrie.Patricia.Map.Enum as TM

import Haschoo.Types   ( Haschoo, Datum(..), ScmValue(ScmPrim, ScmFunc)
                       , idMap, valMap)
import Haschoo.Utils   (ErrOr)

eval :: Datum -> Haschoo ScmValue
eval (Evaluated  v) = return v
eval (UnevaledId s) = do
   ctx <- get
   case msum $ map (liftM2 fmap (,) (TM.lookup s . idMap)) ctx of
        Nothing    -> fail $ "Unbound identifier '" ++ s ++ "'"
        Just (c,i) -> case IM.lookup i (valMap c) of
                           Just v  -> return v
                           Nothing ->
                              throwError $ "Internal error looking up " ++ s

eval (UnevaledApp xs) =
   case xs of
        []   -> fail "Empty application"
        y:ys -> do
           evaledHead <- eval y
           case evaledHead of
                ScmPrim _ f -> f ys
                ScmFunc _ f -> do
                   args   <- mapM eval ys
                   result <- liftIO $ f args
                   case result of
                        Left  s   -> throwError s
                        Right val -> return val

                _           -> throwError "Can't apply non-function"

eval (QuasiQuoted  _)   = throwError "Can't eval quasiquoted yet"
eval (UnQuoted     _)   = throwError "Can't eval unquoted yet"
eval (FlatUnQuoted _)   = throwError "Can't eval flat-unquoted yet"
eval (UnevaledVec  _)   = throwError "Can't eval vector yet"
eval (DottedList   _ _) = throwError "Can't eval dotted yet"
