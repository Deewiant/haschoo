-- File created: 2009-07-15 10:33:14

module Haschoo.Datum where

import Haschoo.ScmValue (ScmValue, scmShow)
import Haschoo.Utils    (showScmList)

data Datum = Evaluated   ScmValue
           | Quoted      Datum
           | QuasiQuoted Datum
           | UnQuoted    Datum
           | UnevaledId  String
           | UnevaledApp [Datum]
           | UnevaledVec [Datum]
           | DottedList  [Datum] Datum
 deriving Show

scmShowDatum :: Datum -> String
scmShowDatum (Evaluated v)     = scmShow v
scmShowDatum (Quoted x)        = '\'': scmShowDatum x
scmShowDatum (QuasiQuoted x)   = '`' : scmShowDatum x
scmShowDatum (UnQuoted x)      = ',' : scmShowDatum x
scmShowDatum (UnevaledId s)    = s
scmShowDatum (UnevaledApp xs)  = showScmList scmShowDatum xs
scmShowDatum (UnevaledVec xs)  = '#' : showScmList scmShowDatum xs
scmShowDatum (DottedList xs x) =
   concat [init (showScmList scmShowDatum xs), " . ", scmShowDatum x, ")"]
