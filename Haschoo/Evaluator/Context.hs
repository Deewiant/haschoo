-- File created: 2009-07-15 10:46:58

module Haschoo.Evaluator.Context where

import           Data.IntMap                     (IntMap)
import qualified Data.IntMap as IM
import           Data.ListTrie.Patricia.Map.Enum (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Enum as TM

import Haschoo.ScmValue             (ScmValue)
import Haschoo.Evaluator.Primitives (primitives)
import Haschoo.Utils                (swap)

data Context = Context { idMap  :: TrieMap Char Int
                       , valMap :: IntMap ScmValue }
 deriving Show

topContext :: Context
topContext = Context ids vals
 where
   key   = zip [0..]
   vals  = IM.fromList .            key . map snd $ primitives
   ids   = TM.fromList . map swap . key . map fst $ primitives
