-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator where

import qualified Data.IntMap as IM
import qualified Data.ListTrie.Patricia.Map.Enum as TM

import Haschoo.Datum             (Datum(..), ScmValue(ScmPrim, ScmFunc))
import Haschoo.Utils             (ErrOr)
import Haschoo.Evaluator.Context (Context(..))

eval :: Context -> Datum -> ErrOr ScmValue
eval _   (Evaluated  v) = Right v
eval ctx (UnevaledId s) =
   case TM.lookup s (idMap ctx) of
        Nothing -> fail $ "Unbound identifier " ++ s
        Just i  -> case IM.lookup i (valMap ctx) of
                        Nothing -> fail $ "Internal error looking up " ++ s
                        Just v  -> Right v

eval ctx (UnevaledApp xs) =
   let es = mapM (eval ctx) xs
    in case es of
            Left s                   -> fail s
            Right []                 -> fail "Empty application"
            Right (ScmFunc _ f : as) -> f as
            Right _                  -> fail "Can't apply non-function"

eval _ (Quoted       _)   = fail "Can't eval quoted yet"
eval _ (QuasiQuoted  _)   = fail "Can't eval quasiquoted yet"
eval _ (UnQuoted     _)   = fail "Can't eval unquoted yet"
eval _ (FlatUnQuoted _)   = fail "Can't eval flat-unquoted yet"
eval _ (UnevaledVec  _)   = fail "Can't eval vector yet"
eval _ (DottedList   _ _) = fail "Can't eval dotted yet"
