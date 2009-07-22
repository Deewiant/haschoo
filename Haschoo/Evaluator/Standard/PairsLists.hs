-- File created: 2009-07-21 22:13:29

module Haschoo.Evaluator.Standard.PairsLists (procedures) where

import Control.Arrow ((&&&))
import Control.Monad (join, replicateM, (>=>))
import Data.IORef    (newIORef, readIORef, writeIORef)

import Haschoo.Types           (ScmValue(..))
import Haschoo.Utils           (ErrOr, ($<))
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("pair?", return . fmap ScmBool . scmIsPair)
   , ("cons",  scmCons)
   , "car" $< id &&& scmCar
   , "cdr" $< id &&& scmCdr
   , ("set-car!", scmSetCar)
   , ("set-cdr!", scmSetCdr) ]
   ++ carCdrCompositions

scmIsPair :: [ScmValue] -> ErrOr Bool
scmIsPair [ScmPair _ _] = Right True
scmIsPair [ScmList _]   = Right True
scmIsPair [_]           = Right False
scmIsPair []            = tooFewArgs  "pair?"
scmIsPair _             = tooManyArgs "pair?"

scmCons :: [ScmValue] -> IO (ErrOr ScmValue)
scmCons [a, b]  = do
   x <- newIORef a
   y <- newIORef b
   return.Right $ ScmPair x y
scmCons (_:_:_) = return$ tooManyArgs "cons"
scmCons _       = return$ tooFewArgs  "cons"

scmCar, scmCdr :: String -> [ScmValue] -> IO (ErrOr ScmValue)
scmCar _ [ScmPair a _]           = return . Right =<< readIORef a
scmCar _ [ScmList (a:_)]         = return . Right $ a
scmCar _ [ScmDottedList (a:_) _] = return . Right $ a
scmCar s [ScmList []]            = return . fail $ s ++ " :: empty list"
scmCar s [_]                     = notPair s
scmCar s []                      = return$ tooFewArgs  s
scmCar s _                       = return$ tooManyArgs s

scmCdr _ [ScmPair _ b]           = return . Right =<< readIORef b
scmCdr _ [ScmList (_:bs)]        = return . Right $ ScmList bs
scmCdr _ [ScmDottedList [_]   b] = return . Right $ b
scmCdr _ [ScmDottedList (_:a) b] = return . Right $ ScmDottedList a b
scmCdr s [ScmList []]            = return . fail $ s ++ " :: empty list"
scmCdr s [_]                     = notPair s
scmCdr s []                      = return$ tooFewArgs  s
scmCdr s _                       = return$ tooManyArgs s

scmSetCar, scmSetCdr :: [ScmValue] -> IO (ErrOr ScmValue)
scmSetCar [ScmPair a _, v] = writeIORef a v >> return (Right ScmVoid)
scmSetCar [_,_]            =         notPair     "set-car!"
scmSetCar (_:_:_)          = return$ tooManyArgs "set-car!"
scmSetCar _                = return$ tooFewArgs  "set-car!"

scmSetCdr [ScmPair _ b, v] = writeIORef b v >> return (Right ScmVoid)
scmSetCdr [_,_]            =         notPair     "set-cdr!"
scmSetCdr (_:_:_)          = return$ tooManyArgs "set-cdr!"
scmSetCdr _                = return$ tooFewArgs  "set-cdr!"

notPair :: String -> IO (ErrOr a)
notPair = return . Left . ("Nonpair argument to " ++)

carCdrCompositions :: [(String, [ScmValue] -> IO (ErrOr ScmValue))]
carCdrCompositions = map (name &&& join func)
                         (concatMap (flip replicateM "ad") [2..depth])
 where
   -- R5RS 6.3.2
   depth = 4

   name a = concat ["c",a,"r"]

   func :: String -> String -> [ScmValue] -> IO (ErrOr ScmValue)
   func _ []       = return . Right . head
   func s ('a':xs) = func s xs >=> next s scmCar
   func s ('d':xs) = func s xs >=> next s scmCdr

   next _ _ x@(Left _) = return x
   next s f  (Right v) = f (name s) [v]
