-- File created: 2009-07-20 13:30:25

module Haschoo.Evaluator.Standard.IO (procedures) where

import Haschoo.Types           (ScmValue(..), scmShow)
import Haschoo.Utils           (ErrOr, showScmList)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("write",   scmWrite)
   , ("display", scmDisplay)
   , ("newline", scmNewline)
   ]

scmWrite, scmDisplay :: [ScmValue] -> IO (ErrOr ScmValue)
scmWrite   = scmPrint scmShow "write"
scmDisplay = scmPrint f "display"
 where
   f (ScmString s) = return s
   f (ScmChar   c) = return [c]
   f (ScmList   l) = showScmList f l
   f x             = scmShow x

-- TODO: always goes to stdout
scmPrint :: (ScmValue -> IO String) -> String -> [ScmValue] -> IO (ErrOr ScmValue)
scmPrint f _ [x]   = f x >>= putStr >> return (Right ScmVoid)
scmPrint _ s [_,_] = return.Left $ s ++ " :: output to port not implemented"
scmPrint _ s []    = return $ tooFewArgs  s
scmPrint _ s _     = return $ tooManyArgs s

scmNewline :: [ScmValue] -> IO (ErrOr ScmValue)
scmNewline [] = putStrLn "" >> return (Right ScmVoid)
scmNewline _  = return $ tooManyArgs "newline"
