-- File created: 2009-07-11 21:47:19

module Haschoo.ScmValue where

data ScmValue = Unevaluated   String
              | ScmBool       Bool
              | ScmChar       Char
              | ScmString     String
              | ScmInt        Integer
              | ScmList       [ScmValue]
              | ScmDottedList [ScmValue] ScmValue
              | ScmVector     [ScmValue]
              | ScmQuoted     ScmValue
 deriving Show
