-- File created: 2009-07-21 22:13:29

module Haschoo.Evaluator.Standard.PairsLists (procedures) where

import Data.IORef          (newIORef, readIORef, writeIORef)

import Haschoo.Types           (ScmValue(..))
import Haschoo.Utils           (ErrOr)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("pair?",    return . fmap ScmBool . scmIsPair)
   , ("cons",     scmCons)
   , ("car",      scmCar)
   , ("cdr",      scmCdr)
   , ("set-car!", scmSetCar)
   , ("set-cdr!", scmSetCdr) ]

scmIsPair :: [ScmValue] -> ErrOr Bool
scmIsPair [ScmPair _ _] = Right True
scmIsPair [ScmList _]   = Right True
scmIsPair [_]           = Right False
scmIsPair []            = tooFewArgs  "pair?"
scmIsPair _             = tooManyArgs "pair?"

scmCons :: [ScmValue] -> IO (ErrOr ScmValue)
scmCons [a, ScmList       b]   = return.Right $ ScmList       (a:b)
scmCons [a, ScmDottedList b c] = return.Right $ ScmDottedList (a:b) c
scmCons [a, b]                 = do
   x <- newIORef a
   y <- newIORef b
   return.Right $ ScmPair x y
scmCons (_:_:_)                = return$ tooManyArgs "cons"
scmCons _                      = return$ tooFewArgs  "cons"

scmCar, scmCdr :: [ScmValue] -> IO (ErrOr ScmValue)
scmCar [ScmPair a _]           = return . Right =<< readIORef a
scmCar [ScmList (a:_)]         = return . Right $ a
scmCar [ScmDottedList (a:_) _] = return . Right $ a
scmCar [ScmList []]            = fail "car :: empty list"
scmCar [_]                     = notPair "car"
scmCar []                      = return$ tooFewArgs  "car"
scmCar _                       = return$ tooManyArgs "car"

scmCdr [ScmPair _ b]           = return . Right =<< readIORef b
scmCdr [ScmList (_:bs)]        = return . Right $ ScmList bs
scmCdr [ScmDottedList [_]   b] = return . Right $ b
scmCdr [ScmDottedList (_:a) b] = return . Right $ ScmDottedList a b
scmCdr [ScmList []]            = fail "cdr :: empty list"
scmCdr [_]                     = notPair "cdr"
scmCdr []                      = return$ tooFewArgs  "cdr"
scmCdr _                       = return$ tooManyArgs "cdr"

scmSetCar :: [ScmValue] -> IO (ErrOr ScmValue)
scmSetCar [ScmPair a _, v] = writeIORef a v >> return (Right ScmVoid)
scmSetCar [_,_]            =         notPair     "set-car!"
scmSetCar (_:_:_)          = return$ tooManyArgs "set-car!"
scmSetCar _                = return$ tooFewArgs  "set-car!"

scmSetCdr :: [ScmValue] -> IO (ErrOr ScmValue)
scmSetCdr [ScmPair _ b, v] = writeIORef b v >> return (Right ScmVoid)
scmSetCdr [_,_]            =         notPair     "set-cdr!"
scmSetCdr (_:_:_)          = return$ tooManyArgs "set-cdr!"
scmSetCdr _                = return$ tooFewArgs  "set-cdr!"

notPair :: String -> IO (ErrOr a)
notPair = return . Left . ("Nonpair argument to " ++)
