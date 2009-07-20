-- File created: 2009-07-20 13:30:25

module Haschoo.Evaluator.Standard.IO (procedures) where

import Haschoo.Types           (ScmValue(..), scmShow)
import Haschoo.Utils           (ErrOr, showScmList)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("write",   scmWrite)
   , ("display", scmDisplay)
   ]

scmWrite, scmDisplay :: [ScmValue] -> IO (ErrOr ScmValue)
scmWrite   = scmPrint scmShow "write"
scmDisplay = scmPrint f "display"
 where
   f (ScmString s) = s
   f (ScmChar   c) = [c]
   f (ScmList   l) = showScmList f l
   f x             = scmShow x

-- TODO: always goes to stdout
scmPrint :: (ScmValue -> String) -> String -> [ScmValue] -> IO (ErrOr ScmValue)
scmPrint f _ [x]   = putStr (f x) >> return (Right ScmVoid)
scmPrint _ s [_,_] = return.Left $ s ++ " :: output to port not implemented"
scmPrint _ s []    = return $ tooFewArgs  s
scmPrint _ s _     = return $ tooManyArgs s
