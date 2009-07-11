-- File created: 2009-07-11 21:47:19

module Haschoo.ScmType where

data ScmType = ScmBool   Bool
             | ScmChar   Char
             | ScmString String
             | ScmInt    Integer
 deriving Show
