-- File created: 2009-07-15 11:00:10

module Haschoo.Evaluator.Standard (context) where

import           Haschoo.Types             (ScmValue, Context, mkContext)
import qualified Haschoo.Evaluator.Standard.Numeric as Numeric

procedures :: [(String, ScmValue)]
procedures = Numeric.procedures

context :: Context
context = mkContext procedures