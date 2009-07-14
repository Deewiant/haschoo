-- File created: 2009-07-11 21:47:19

module Haschoo.ScmValue where

-- The list types:
--   Application: (a b c)
--   DottedList:  (a b . c)
--   ScmList:     (list a b c)
data ScmValue = UnevaledId  String
              | Application [ScmValue]
              | DottedList  [ScmValue] ScmValue
              | ScmBool     Bool
              | ScmChar     Char
              | ScmString   String
              | ScmInt      Integer
              | ScmList     [ScmValue]
              | ScmVector   [ScmValue]
              | ScmQuoted   ScmValue
 deriving Show
