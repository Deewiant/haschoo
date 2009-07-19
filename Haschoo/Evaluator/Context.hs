-- File created: 2009-07-15 10:46:58

module Haschoo.Evaluator.Context (Context(..), mkContext) where

import           Data.IntMap                     (IntMap)
import qualified Data.IntMap as IM
import           Data.ListTrie.Patricia.Map.Enum (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Enum as TM

import Haschoo.Datum (ScmValue)
import Haschoo.Utils (swap)

data Context = Context { idMap  :: TrieMap Char Int
                       , valMap :: IntMap ScmValue }
 deriving Show

mkContext :: [(String, ScmValue)] -> Context
mkContext namedVals = Context ids vals
 where
   key   = zip [0..]
   vals  = IM.fromList .            key . map snd $ namedVals
   ids   = TM.fromList . map swap . key . map fst $ namedVals
