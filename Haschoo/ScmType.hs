-- File created: 2009-07-11 21:47:19

module Haschoo.ScmType where

data ScmType = Unevaluated   String
             | ScmBool       Bool
             | ScmChar       Char
             | ScmString     String
             | ScmInt        Integer
             | ScmList       [ScmType]
             | ScmDottedList [ScmType] ScmType
             | ScmVector     [ScmType]
             | ScmQuoted     ScmType
 deriving Show
