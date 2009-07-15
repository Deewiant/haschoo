-- File created: 2009-07-15 11:00:10

module Haschoo.Evaluator.Primitives (primitives) where

import Haschoo.ScmValue (ScmValue)
import qualified Haschoo.Evaluator.Primitives.Numeric as Numeric

primitives :: [(String, ScmValue)]
primitives = Numeric.primitives
