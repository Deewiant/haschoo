-- File created: 2009-07-14 14:37:46

module Haschoo.Evaluator where

import qualified Data.IntMap as IM
import qualified Data.ListTrie.Patricia.Map.Enum as TM

import Haschoo.Datum             (Datum(..))
import Haschoo.ScmValue          (ScmValue(..))
import Haschoo.Evaluator.Context (Context(..))

eval :: Context -> Datum -> ScmValue
eval _   (Evaluated  v) = v
eval ctx (UnevaledId s) =
   case TM.lookup s (idMap ctx) of
        Nothing -> error $ "Unbound identifier " ++ s
        Just i  -> case IM.lookup i (valMap ctx) of
                        Nothing -> error $ "Internal error looking up " ++ s
                        Just v  -> v

eval ctx (UnevaledApp xs) =
   let es = map (eval ctx) xs
    in case es of
            []                 -> error "Empty application"
            (ScmFunc _ f : as) -> f as
            _                  -> error "Can't apply non-function"

eval _ (Quoted      _)   = error "Can't eval quoted yet"
eval _ (UnevaledVec _)   = error "Can't eval vector yet"
eval _ (DottedList  _ _) = error "Can't eval dotted yet"
