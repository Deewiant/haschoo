-- File created: 2009-07-15 11:00:10

module Haschoo.Evaluator.Standard (procedures) where

import Haschoo.ScmValue (ScmValue)
import qualified Haschoo.Evaluator.Standard.Numeric as Numeric

procedures :: [(String, ScmValue)]
procedures = Numeric.procedures
