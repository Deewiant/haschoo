-- File created: 2009-07-21 22:13:29

module Haschoo.Evaluator.Standard.PairsLists (procedures) where

import Control.Applicative ((<$>))
import Control.Arrow       ((&&&), second)
import Control.Monad       (join, replicateM, (>=>))
import Data.IORef          (IORef, newIORef, readIORef, writeIORef)

import Haschoo.Types           (ScmValue(..))
import Haschoo.Utils           (ErrOr, ($<), ptrEq)
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("pair?", return . fmap ScmBool . scmIsPair)
   , ("cons",  scmCons)
   , "car" $< id &&& scmCar
   , "cdr" $< id &&& scmCdr
   , ("set-car!", scmSetCar)
   , ("set-cdr!", scmSetCdr) ]

   ++ carCdrCompositions ++

   [ ("null?",  return . fmap ScmBool . scmIsNull)
   , ("list?",  fmap (fmap ScmBool) . scmIsList)
   , ("list",   fmap Right . scmList)
   , ("length", scmLength)
   , ("append", scmAppend) ]

---- Pairs

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
   func s _        = error (name s ++ " :: the impossible happened!")

   next _ _ x@(Left _) = return x
   next s f  (Right v) = f (name s) [v]

---- Lists

scmIsNull :: [ScmValue] -> ErrOr Bool
scmIsNull [ScmList []] = Right True
scmIsNull [_]          = Right False
scmIsNull []           = tooFewArgs  "null?"
scmIsNull _            = tooManyArgs "null?"

scmIsList :: [ScmValue] -> IO (ErrOr Bool)
scmIsList [ScmList _]   = return$ Right True
scmIsList [ScmPair _ b] = Right <$> (readIORef b >>= join go)
 where
   go (ScmPair _ sn) fast@(ScmPair _ fn) = do
      fn' <- readIORef fn
      case fn' of
           ScmPair _ fn2 -> do
              eq <- ptrEq fast fn'
              if eq
                 then return False -- Cycle
                 else do
                    fn2' <- readIORef fn2
                    case fn2' of
                         ScmPair _ _ -> readIORef sn >>= flip go fn2'
                         ScmList _   -> return True
                         _           -> return False
           ScmList _ -> return True
           _         -> return False

   go _ (ScmList _) = return True
   go _ _           = return False

scmIsList [_]           = return$ Right False
scmIsList []            = return$ tooFewArgs  "list?"
scmIsList _             = return$ tooManyArgs "list?"

scmList :: [ScmValue] -> IO ScmValue
scmList []     = return (ScmList [])
scmList (x:xs) = do
   ys <- scmList xs
   y  <- newIORef x
   z  <- newIORef ys
   return (ScmPair y z)

scmLength :: [ScmValue] -> IO (ErrOr ScmValue)
scmLength [ScmPair _ p] = readIORef p >>= go 1
 where
   go n _ | n `seq` False = undefined
   go n (ScmList [])  = return . Right . ScmInt . toInteger $ (n :: Int)
   go n (ScmPair _ b) = readIORef b >>= go (n+1)
   go _ _             = return$ notList "length"

scmLength [ScmList xs]  = return . Right . ScmInt . toInteger $ length xs
scmLength [_]           = return$ notList     "length"
scmLength []            = return$ tooFewArgs  "length"
scmLength _             = return$ tooManyArgs "length"

scmAppend :: [ScmValue] -> IO (ErrOr ScmValue)
scmAppend [p@(ScmPair _ _), rhs] = next p
 where
   -- Builds up the list as we iterate through it
   next :: ScmValue -> IO (ErrOr ScmValue)
   next (ScmPair a b) = do
      b' <- go =<< readIORef b
      case b' of
           Left  _ -> return b'
           Right y -> do
              newA <- newIORef =<< readIORef a
              newB <- newIORef y
              return . Right $ ScmPair newA newB

   next _ = error "append :: the impossible happened"

   go :: ScmValue -> IO (ErrOr ScmValue)
   go x@(ScmPair _ _) = next x
   go x@(ScmList _)   = scmAppend [x, rhs]
   go _               = return$ notList "append"

scmAppend [ScmList xs, rhs] = do
   (xs', fin) <- pairify xs
   case fin of
        Nothing -> return . Right $ rhs
        Just p  -> do
           writeIORef p rhs
           return . Right $ xs'

scmAppend [_,_]   = return$ notList     "append"
scmAppend (_:_:_) = return$ tooManyArgs "append"
scmAppend _       = return$ tooFewArgs  "append"

---- Utils

notList :: String -> ErrOr a
notList = fail . ("Nonlist argument to "++)

-- ScmList -> ScmPair, returns the tail pointer if it wasn't empty
pairify :: [ScmValue] -> IO (ScmValue, Maybe (IORef ScmValue))
pairify []     = return (ScmList [], Nothing)
pairify (x:xs) = second Just <$> go x xs
 where
   go :: ScmValue -> [ScmValue] -> IO (ScmValue, IORef ScmValue)
   go v [] = do
      a <- newIORef v
      b <- newIORef (ScmList [])
      return (ScmPair a b, b)

   go v (y:ys) = do
      a         <- newIORef v
      (zs, fin) <- go y ys
      b         <- newIORef zs
      return (ScmPair a b, fin)
