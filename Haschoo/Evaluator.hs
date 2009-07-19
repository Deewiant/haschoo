-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator where

import           Control.Monad (liftM2, msum)
import qualified Data.IntMap as IM
import qualified Data.ListTrie.Patricia.Map.Enum as TM

import Haschoo.Types (Datum(..), ScmValue(ScmPrim, ScmFunc), Context(..))
import Haschoo.Utils (ErrOr)

eval :: [Context] -> Datum -> ErrOr ScmValue
eval _   (Evaluated  v) = Right v
eval ctx (UnevaledId s) =
   case msum $ map (liftM2 fmap (,) (TM.lookup s . idMap)) ctx of
        Nothing    -> fail $ "Unbound identifier " ++ s
        Just (c,i) -> case IM.lookup i (valMap c) of
                           Nothing -> fail $ "Internal error looking up " ++ s
                           Just v  -> Right v

eval ctx (UnevaledApp xs) =
   case xs of
        []   -> fail "Empty application"
        y:ys ->
           case eval ctx y of
                Left  s             -> fail s
                Right (ScmPrim _ f) -> f ctx ys
                Right (ScmFunc _ f) -> case mapM (eval ctx) ys of
                                            Left  s    -> fail s
                                            Right args -> f args
                Right _             -> fail "Can't apply non-function"

eval _ (Quoted       _)   = fail "Can't eval quoted yet"
eval _ (QuasiQuoted  _)   = fail "Can't eval quasiquoted yet"
eval _ (UnQuoted     _)   = fail "Can't eval unquoted yet"
eval _ (FlatUnQuoted _)   = fail "Can't eval flat-unquoted yet"
eval _ (UnevaledVec  _)   = fail "Can't eval vector yet"
eval _ (DottedList   _ _) = fail "Can't eval dotted yet"
