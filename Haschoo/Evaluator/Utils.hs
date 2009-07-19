-- File created: 2009-07-19 18:41:22

module Haschoo.Evaluator.Utils where

import Haschoo.Utils (ErrOr)

tooFewArgs, tooManyArgs :: String -> ErrOr a
tooFewArgs  = fail . ("Too few arguments to " ++)
tooManyArgs = fail . ("Too many arguments to " ++)
