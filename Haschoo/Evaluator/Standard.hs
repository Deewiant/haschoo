-- File created: 2009-07-15 11:00:10

module Haschoo.Evaluator.Standard (context) where

import           Haschoo.Types (ScmValue, Context, mkContext)
import qualified Haschoo.Evaluator.Standard.Boolean     as Boolean
import qualified Haschoo.Evaluator.Standard.Characters  as Characters
import qualified Haschoo.Evaluator.Standard.Control     as Control
import qualified Haschoo.Evaluator.Standard.Equivalence as Equivalence
import qualified Haschoo.Evaluator.Standard.Eval        as Eval
import qualified Haschoo.Evaluator.Standard.IO          as IO
import qualified Haschoo.Evaluator.Standard.Numeric     as Numeric
import qualified Haschoo.Evaluator.Standard.PairsLists  as PairsLists
import qualified Haschoo.Evaluator.Standard.Strings     as Strings
import qualified Haschoo.Evaluator.Standard.Symbols     as Symbols
import qualified Haschoo.Evaluator.Standard.Vectors     as Vectors

procedures :: [(String, ScmValue)]
procedures = concat [ Boolean.procedures
                    , Characters.procedures
                    , Control.procedures
                    , Equivalence.procedures
                    , Eval.procedures
                    , IO.procedures
                    , Numeric.procedures
                    , PairsLists.procedures
                    , Strings.procedures
                    , Symbols.procedures
                    , Vectors.procedures ]

context :: Context
context = mkContext procedures
