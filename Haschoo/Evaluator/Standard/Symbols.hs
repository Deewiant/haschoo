-- File created: 2009-07-22 20:39:04

module Haschoo.Evaluator.Standard.Symbols (procedures) where

import Data.Array.IArray (elems)
import Data.Array.MArray (getElems)

import Haschoo.Types           (ScmValue(..), toScmString, toScmMString)
import Haschoo.Utils           (ErrOr)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b))
   [ ("symbol?",        return . fmap ScmBool . scmIsSymbol)
   , ("symbol->string", return . scmToString)
   , ("string->symbol", scmToSymbol)
   ]

scmIsSymbol :: [ScmValue] -> ErrOr Bool
scmIsSymbol [ScmIdentifier _] = Right True
scmIsSymbol [_]               = Right False
scmIsSymbol []                = tooFewArgs  "symbol?"
scmIsSymbol _                 = tooManyArgs "symbol?"

scmToString :: [ScmValue] -> ErrOr ScmValue
scmToString [ScmIdentifier x] = Right (toScmString x)
scmToString [_]               = notSymbol   "symbol->string"
scmToString []                = tooFewArgs  "symbol->string"
scmToString _                 = tooManyArgs "symbol->string"

scmToSymbol :: [ScmValue] -> IO (ErrOr ScmValue)
scmToSymbol [ScmString x]  = return$ Right . ScmIdentifier  $    elems x
scmToSymbol [ScmMString x] = fmap   (Right . ScmIdentifier) $ getElems x
scmToSymbol [_]            = return$ notSymbol   "string->symbol"
scmToSymbol []             = return$ tooFewArgs  "string->symbol"
scmToSymbol _              = return$ tooManyArgs "string->symbol"

notSymbol :: String -> ErrOr a
notSymbol = fail . ("Nonsymbolic argument to " ++)
