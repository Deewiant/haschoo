-- File created: 2009-07-21 22:04:14

module Haschoo.Evaluator.Standard.Boolean (procedures) where

import Haschoo.Types           (ScmValue(..))
import Haschoo.Utils           (ErrOr)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a (return. b))) $
   [ ("not",      fmap ScmBool . scmNot)
   , ("boolean?", fmap ScmBool . scmIsBoolean) ]

scmNot, scmIsBoolean :: [ScmValue] -> ErrOr Bool
scmNot [ScmBool False] = Right True
scmNot [_]             = Right False
scmNot []              = tooFewArgs  "not"
scmNot _               = tooManyArgs "not"

scmIsBoolean [ScmBool _] = Right True
scmIsBoolean [_]         = Right False
scmIsBoolean []          = tooFewArgs  "boolean?"
scmIsBoolean _           = tooManyArgs "boolean?"
