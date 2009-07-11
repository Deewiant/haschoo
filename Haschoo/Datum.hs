-- File created: 2009-07-11 20:33:49

module Haschoo.Datum where

import Haschoo.ScmType (ScmType)

data Datum = ParsedIdentifier String
           | Sema ScmType
           | List [Datum]
           | DottedList [Datum] Datum
           | Vector [Datum]
           | Quoted Datum
 deriving Show
