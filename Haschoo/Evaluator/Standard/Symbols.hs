-- File created: 2009-07-22 20:39:04

module Haschoo.Evaluator.Standard.Symbols (procedures) where

import Haschoo.Types           (ScmValue(..))
import Haschoo.Utils           (ErrOr)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a (return . b))) $
   [ ("symbol?",        fmap ScmBool . scmIsSymbol)
   , ("symbol->string", scmToString)
   , ("string->symbol", scmToSymbol)
   ]

scmIsSymbol :: [ScmValue] -> ErrOr Bool
scmIsSymbol [ScmIdentifier _] = Right True
scmIsSymbol [_]               = Right False
scmIsSymbol []                = tooFewArgs  "symbol?"
scmIsSymbol _                 = tooManyArgs "symbol?"

scmToString, scmToSymbol :: [ScmValue] -> ErrOr ScmValue
scmToString [ScmIdentifier x] = Right$ ScmString x
scmToString [_]               = fail "Nonsymbolic argument to symbol->string"
scmToString []                = tooFewArgs  "symbol->string"
scmToString _                 = tooManyArgs "symbol->string"

scmToSymbol [ScmString x] = Right$ ScmIdentifier x
scmToSymbol [_]           = fail "Nonsymbolic argument to string->symbol"
scmToSymbol []            = tooFewArgs  "string->symbol"
scmToSymbol _             = tooManyArgs "string->symbol"
