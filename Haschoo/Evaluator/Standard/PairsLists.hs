-- File created: 2009-07-21 22:13:29

module Haschoo.Evaluator.Standard.PairsLists (procedures, scmAppend) where

import Control.Applicative ((<$>))
import Control.Arrow       ((&&&))
import Control.Monad       (join, replicateM, (>=>))
import Control.Monad.Loops (dropWhileM, firstM)
import Data.Foldable       (foldrM)
import Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import Data.List           (genericDrop)
import Data.Maybe          (fromMaybe)

import Haschoo.Types                          ( ScmValue(..)
                                              , pairToList, listToPair)
import Haschoo.Utils                          (ErrOr, ($<), ptrEq)
import Haschoo.Evaluator.Utils                (tooFewArgs,tooManyArgs, notList)
import Haschoo.Evaluator.Standard.Equivalence (scmEq, scmEqv, scmEqual)
import Haschoo.Evaluator.Standard.Numeric     (asInt)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b)) $
   [ ("pair?", return . fmap ScmBool . scmIsPair)
   , ("cons",  scmCons)
   , "car" $< id &&& scmCar
   , "cdr" $< id &&& scmCdr
   , ("set-car!", scmSetCar)
   , ("set-cdr!", scmSetCdr) ]

   ++ carCdrCompositions ++

   [ ("null?",    return . fmap ScmBool . scmIsNull)
   , ("list?",    fmap (fmap ScmBool) . scmIsList)
   , ("list",     fmap Right . scmList)
   , ("length",   scmLength)
   , ("append",   scmAppend)
   , ("reverse",  scmReverse)
   , ("list-ref", scmListRef)
   , ("memq",     scmMemq)
   , ("memv",     scmMemv)
   , ("member",   scmMember)
   , ("assq",     scmAssq)
   , ("assv",     scmAssv)
   , ("assoc",    scmAssoc) ]

---- Pairs

--- pair?

scmIsPair :: [ScmValue] -> ErrOr Bool
scmIsPair [ScmPair _ _]       = Right True
scmIsPair [ScmList (_:_)]     = Right True
scmIsPair [ScmDottedList _ _] = Right True
scmIsPair [_]                 = Right False
scmIsPair []                  = tooFewArgs  "pair?"
scmIsPair _                   = tooManyArgs "pair?"

--- cons

scmCons :: [ScmValue] -> IO (ErrOr ScmValue)
scmCons [a, b]  = do
   x <- newIORef a
   y <- newIORef b
   return.Right $ ScmPair x y
scmCons (_:_:_) = return$ tooManyArgs "cons"
scmCons _       = return$ tooFewArgs  "cons"

--- car cdr

scmCar, scmCdr :: String -> [ScmValue] -> IO (ErrOr ScmValue)
scmCar _ [ScmPair a _]           = return . Right =<< readIORef a
scmCar _ [ScmList (a:_)]         = return . Right $ a
scmCar _ [ScmDottedList (a:_) _] = return . Right $ a
scmCar s [ScmList []]            = return . fail $ s ++ " :: empty list"
scmCar s [_]                     = return$ notPair s
scmCar s []                      = return$ tooFewArgs  s
scmCar s _                       = return$ tooManyArgs s

scmCdr _ [ScmPair _ b]           = return . Right =<< readIORef b
scmCdr _ [ScmList (_:bs)]        = return . Right $ ScmList bs
scmCdr _ [ScmDottedList [_]   b] = return . Right $ b
scmCdr _ [ScmDottedList (_:a) b] = return . Right $ ScmDottedList a b
scmCdr s [ScmList []]            = return . fail $ s ++ " :: empty list"
scmCdr s [_]                     = return$ notPair s
scmCdr s []                      = return$ tooFewArgs  s
scmCdr s _                       = return$ tooManyArgs s

--- set-car! set-cdr!

scmSetCar, scmSetCdr :: [ScmValue] -> IO (ErrOr ScmValue)
scmSetCar = scmSet (const . writeIORef) "set-car!"
scmSetCdr = scmSet (const   writeIORef) "set-cdr!"

scmSet :: (IORef ScmValue -> IORef ScmValue -> ScmValue -> IO ()) -> String
       -> [ScmValue] -> IO (ErrOr ScmValue)

scmSet f _ [ScmPair a b, v]       = f a b v >> return (Right ScmVoid)
scmSet _ s [ScmList _, _]         = return$ immutable   s
scmSet _ s [ScmDottedList _ _, _] = return$ immutable   s
scmSet _ s [_,_]                  = return$ notPair     s
scmSet _ s (_:_:_)                = return$ tooManyArgs s
scmSet _ s _                      = return$ tooFewArgs  s

--- caar cadr ... cddddr

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

--- null? list?

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

--- list

scmList :: [ScmValue] -> IO ScmValue
scmList []     = return (ScmList [])
scmList (x:xs) = do
   ys <- scmList xs
   y  <- newIORef x
   z  <- newIORef ys
   return (ScmPair y z)

--- length

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

--- append

scmAppend :: [ScmValue] -> IO (ErrOr ScmValue)
scmAppend args@(_:_) = foldrM fold (Left "") args
 where
   -- The last argument is allowed to be a nonlist: this is a bit of a hack to
   -- catch that case
   --
   -- If we were to just match on ScmList [] in f, we'd allow an arbitrary
   -- number of '() to follow the final argument
   fold x (Left "") = return (Right x)

   fold _ err@(Left _) = return err
   fold a (Right b)    = f a b

   f (p@(ScmPair _ _)) rhs = next rhs p
   f (ScmList xs)      rhs = do
      (xs', fin) <- listToPair xs
      case fin of
           Nothing -> return . Right $ rhs
           Just p  -> do
              writeIORef p rhs
              return . Right $ xs'

   f _ _ = return$ notList "append"

   -- Builds up the list as we iterate through it
   next rhs (ScmPair a b) = do
      b' <- go rhs =<< readIORef b
      case b' of
           Left  _ -> return b'
           Right y -> do
              newA <- newIORef =<< readIORef a
              newB <- newIORef y
              return . Right $ ScmPair newA newB

   next _ _ = error "append :: the impossible happened"

   go rhs x@(ScmPair _ _) = next rhs x
   go rhs x@(ScmList _)   = f x rhs
   go _   _               = return$ notList "append"

scmAppend [] = return$ tooFewArgs "append"

--- reverse

scmReverse :: [ScmValue] -> IO (ErrOr ScmValue)
scmReverse [ScmList xs]      = fmap (Right . fst) . listToPair $ reverse xs
scmReverse [x@(ScmPair _ _)] = do
   x' <- pairToList x
   case x' of
        Right (ScmList l) -> fmap (Right . fst) . listToPair $ reverse l
        Left _            -> return $ notList "reverse"
        Right _           -> error "reverse :: the impossible happened"
scmReverse [_]               = return$ notList     "reverse"
scmReverse []                = return$ tooFewArgs  "reverse"
scmReverse _                 = return$ tooManyArgs "reverse"

--- list-ref

scmListRef :: [ScmValue] -> IO (ErrOr ScmValue)
scmListRef [x, n] =
   case fmap snd $ asInt "list-ref" n of
        Right i -> f x i
        Left  s -> return (Left s)
 where
   f (ScmList xs)  i = return$ case genericDrop i xs of
                                    []  -> fail "list-ref :: overlarge index"
                                    v:_ -> Right v

   f (ScmPair a _) 0 = Right <$> readIORef a
   f (ScmPair _ b) i = readIORef b >>= flip f (i-1)
   f _             _ = return$ notList "list-ref"

scmListRef (_:_:_) = return$ tooManyArgs "list-ref"
scmListRef _       = return$ tooFewArgs  "list-ref"

--- memq memv member

scmMemq, scmMemv, scmMember :: [ScmValue] -> IO (ErrOr ScmValue)
scmMemq   = scmFind scmEq    "memq"
scmMemv   = scmFind scmEqv   "memv"
scmMember = scmFind scmEqual "member"

scmFind :: (ScmValue -> ScmValue -> IO Bool) -> String
        -> [ScmValue] -> IO (ErrOr ScmValue)
scmFind p s [obj, list] =
   case list of
        ScmList _   -> go list
        ScmPair _ _ -> go list
        _           -> return$ notList s
 where
   go (ScmList xs) = do
      ys <- dropWhileM (fmap not . p obj) xs
      return.Right $ if null ys
                        then ScmBool False
                        else ScmList ys

   go x@(ScmPair a b) = do
      found <- p obj =<< readIORef a
      if found
         then return . Right $ x
         else readIORef b >>= go

   go _ = return$ notList s

scmFind _ s (_:_:_) = return$ tooManyArgs s
scmFind _ s _       = return$ tooFewArgs  s

--- assq assv assoc

scmAssq, scmAssv, scmAssoc :: [ScmValue] -> IO (ErrOr ScmValue)
scmAssq  = scmLookup scmEq    "assq"
scmAssv  = scmLookup scmEqv   "assv"
scmAssoc = scmLookup scmEqual "assoc"

scmLookup :: (ScmValue -> ScmValue -> IO Bool) -> String
          -> [ScmValue] -> IO (ErrOr ScmValue)
scmLookup p s [obj, list] =
   case list of
        ScmList _   -> go list
        ScmPair _ _ -> go list
        _           -> return$ notList s
 where
   go (ScmList xs) = Right . fromMaybe (ScmBool False) <$> firstM match xs
   go (ScmPair a b) = do
      a'    <- readIORef a
      found <- match a'
      if found
         then return . Right $ a'
         else readIORef b >>= go

   go _ = return$ notList s

   match (ScmPair a _)           = p obj =<< readIORef a
   match (ScmDottedList (a:_) _) = p obj a
   match (ScmList (a:_))         = p obj a
   match _                       = return False

scmLookup _ s (_:_:_) = return$ tooManyArgs s
scmLookup _ s _       = return$ tooFewArgs  s

---- Utils

immutable, notPair :: String -> ErrOr a
immutable = fail . ("Immutable argument to " ++)
notPair   = fail . ("Nonpair argument to " ++)
