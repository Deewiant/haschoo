-- File created: 2009-08-05 20:14:14

module Haschoo.Evaluator.Standard.Strings (procedures, isString, strToList)
 where

import Control.Applicative ((<$>))
import Control.Arrow       ((&&&))
import Control.Monad       (join, liftM2)
import Data.Array.IArray   ((!), bounds, elems)
import Data.Array.IO       (IOUArray)
import Data.Array.MArray   ( readArray, writeArray
                           , newArray, newArray_, newListArray
                           , getBounds, getElems
                           , freeze, thaw, unsafeFreeze, unsafeThaw)
import Data.Array.Unboxed  (UArray)
import Data.Char           (isLetter, toLower)
import Data.Foldable       (foldrM)
import Data.Function       (on)

import Haschoo.Types           ( ScmValue(..), toScmMString
                               , listToPair, pairToList)
import Haschoo.Utils           (ErrOr, ($<), (.:), mapOnPairs)
import Haschoo.Evaluator.Utils ( tooFewArgs, tooManyArgs, immutable
                               , notChar, notInt, notList, notString)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b))
   [ ("string?", return . fmap ScmBool . scmIsString)

   , ("make-string", scmMakeString)
   , ("string",      scmString)

   , ("string-length", scmLength)

   , ("string-ref",  scmRef)
   , ("string-set!", scmSet)

   , "string=?"  $< id &&& fmap (fmap ScmBool) .: scmCompare (==) (,)
   , "string<?"  $< id &&& fmap (fmap ScmBool) .: scmCompare (<)  (,)
   , "string>?"  $< id &&& fmap (fmap ScmBool) .: scmCompare (>)  (,)
   , "string<=?" $< id &&& fmap (fmap ScmBool) .: scmCompare (<=) (,)
   , "string>=?" $< id &&& fmap (fmap ScmBool) .: scmCompare (>=) (,)

   , "string-ci=?"  $< id &&& fmap (fmap ScmBool) .: scmCompare (==) lower
   , "string-ci<?"  $< id &&& fmap (fmap ScmBool) .: scmCompare (<)  lower
   , "string-ci>?"  $< id &&& fmap (fmap ScmBool) .: scmCompare (>)  lower
   , "string-ci<=?" $< id &&& fmap (fmap ScmBool) .: scmCompare (<=) lower
   , "string-ci>=?" $< id &&& fmap (fmap ScmBool) .: scmCompare (>=) lower

   , ("substring", scmSubstring)

   , ("string-append", scmAppend)

   , ("string->list", scmToList)
   , ("list->string", scmFromList)

   , ("string-copy",  scmCopy)
   , ("string-fill!", scmFill)
   ]
 where
   lower a b = if isLetter a && isLetter b
                  then (toLower a, toLower b)
                  else (a,b)

scmIsString :: [ScmValue] -> ErrOr Bool
scmIsString [x] = Right (isString x)
scmIsString []  = tooFewArgs  "string?"
scmIsString _   = tooManyArgs "string?"

scmMakeString, scmString :: [ScmValue] -> IO (ErrOr ScmValue)
scmMakeString (ScmInt l : xs) =
   case xs of
        []          -> f newArray_
        [ScmChar c] -> f (flip newArray c)
        [_]         -> return$ notChar     "make-string"
        _           -> return$ tooManyArgs "make-string"
 where
   f mk = tryToLen "make-string" l $ \i -> ScmMString <$> mk (0, i-1)

scmMakeString [_, ScmChar _] = return$ notInt      "make-string"
scmMakeString (_:_:_:_)      = return$ tooManyArgs "make-string"
scmMakeString []             = return$ tooFewArgs  "make-string"
scmMakeString _              = return$ notChar     "make-string"

scmString xs = case mapM (fromChar "string") xs of
                    Left  e -> return$ Left e
                    Right s -> fmap Right $ toScmMString s

scmLength :: [ScmValue] -> IO (ErrOr ScmValue)
scmLength [x] = case x of
                     ScmString  s -> Right . ScmInt . toInteger <$>  sLen s
                     ScmMString s -> Right . ScmInt . toInteger <$> msLen s
                     _            -> return$ notString "string-length"

scmLength [] = return$ tooFewArgs  "string-length"
scmLength _  = return$ tooManyArgs "string-length"

scmRef, scmSet :: [ScmValue] -> IO (ErrOr ScmValue)
scmRef [x, ScmInt i] = case x of
                            ScmString  s -> f (sLen  s) (return . (s!))
                            ScmMString s -> f (msLen s) (readArray s)
                            _            -> return$ notString "string-ref"
 where
   f len = fmap (fmap ScmChar) . tryToIdx "string-ref" len i

scmRef [_,_]   = return$ notInt      "string-ref"
scmRef (_:_:_) = return$ tooManyArgs "string-ref"
scmRef _       = return$ tooFewArgs  "string-ref"

scmSet [x, ScmInt idx, ScmChar c] =
   case x of
        ScmMString s -> do
           tryToIdx "string-set!" (msLen s) idx (\i -> writeArray s i c)
           return (Right ScmVoid)

        ScmString  _ -> return$ immutable "string-set!"
        _            -> return$ notString "string-set!"

scmSet [_, _, ScmChar _] = return$ notInt      "string-set!"
scmSet [_, _, _]         = return$ notChar     "string-set!"
scmSet (_:_:_:_)         = return$ tooManyArgs "string-set!"
scmSet _                 = return$ tooFewArgs  "string-set!"

scmCompare :: (String -> String -> Bool)
           -> (Char -> Char -> (Char, Char))
           -> String
           -> [ScmValue] -> IO (ErrOr Bool)
scmCompare p f s [a, b] =
   if isString a && isString b
      then Right <$> (liftM2 (uncurry p .: mapOnPairs f `on` elems) `on` conv)
                     a b
      else return$ notString s
 where
   conv (ScmString  x) = return x
   conv (ScmMString x) = unsafeFreeze x
   conv _              = error $ s ++ " :: the impossible happpened"
scmCompare _ _ s (_:_:_) = return$ tooManyArgs s
scmCompare _ _ s _       = return$ tooFewArgs  s

scmSubstring :: [ScmValue] -> IO (ErrOr ScmValue)
scmSubstring [x, ScmInt a, ScmInt b] =
   case x of
        ScmMString s -> f (msLen s) (getElems s)
        ScmString  s -> f (sLen s) (return $ elems s)
        _            -> return$ notString "substring"
 where
   f mlen els = mlen >>= \len ->
      fmap join $
      tryRange 0 (toInteger len) "substring" b $ \b' ->
      tryRange 0 (toInteger b')  "substring" a $ \a' ->
      fmap ScmMString $ els >>= newListArray (0, b'-a'-1) . drop a'

scmSubstring [_, _, _] = return$ notInt      "substring"
scmSubstring (_:_:_:_) = return$ tooManyArgs "substring"
scmSubstring _         = return$ tooFewArgs  "substring"

scmAppend :: [ScmValue] -> IO (ErrOr ScmValue)
scmAppend args = do
   x <- foldrM f (Right (0, "")) args
   case x of
        Left  err        -> return (Left err)
        Right (len, cat) -> Right . ScmMString <$> newListArray (0, len-1) cat
 where
   f (ScmString  a) (Right x)  = make  (sLen a) (return $ elems a) x
   f (ScmMString a) (Right x)  = make (msLen a) (getElems a)       x
   f _              (Left err) = return (Left err)
   f _              _          = return$ notString "string-append"

   make ml me (n,s) = ml >>= \l -> me >>= \e -> return.Right $ (n+l, e++s)

scmToList, scmFromList :: [ScmValue] -> IO (ErrOr ScmValue)
scmToList [x] | isString x =
   fmap (Right . fst) . listToPair . map ScmChar =<< strToList x

scmToList [_] = return$ notString   "string->list"
scmToList []  = return$ tooFewArgs  "string->list"
scmToList _   = return$ tooManyArgs "string->list"

scmFromList [x] =
   case x of
        ScmList l       -> f l
        p@(ScmPair _ _) -> do
           l <- pairToList p
           case l of
                Left _             -> return$ notList "list->string"
                Right (ScmList l') -> f l'

                _ -> error "list->string :: the impossible happened"

        _ -> return$ notList "list->string"
 where
   f l = case mapM (fromChar "list->string") l of
              Left  e -> return$ Left e
              Right s -> Right <$> toScmMString s

scmFromList [] = return$ tooFewArgs  "list->string"
scmFromList _  = return$ tooManyArgs "list->string"

scmCopy :: [ScmValue] -> IO (ErrOr ScmValue)
scmCopy [ScmString  s] = Right . ScmMString <$> thaw s
scmCopy [ScmMString s] =
   Right . ScmMString <$>
      (freeze s >>= \a -> unsafeThaw (a :: UArray Int Char))

scmCopy [_] = return$ notString   "string-copy"
scmCopy []  = return$ tooFewArgs  "string-copy"
scmCopy _   = return$ tooManyArgs "string-copy"

scmFill :: [ScmValue] -> IO (ErrOr ScmValue)
scmFill [x, ScmChar c] =
   case x of
        ScmMString s -> do
           (a,b) <- getBounds s
           mapM_ (\i -> writeArray s i c) [a..b]
           return (Right ScmVoid)

        ScmString  _ -> return$ immutable "string-fill!"
        _            -> return$ notString "string-fill!"

scmFill [_, _]  = return$ notChar     "string-fill!"
scmFill (_:_:_) = return$ tooManyArgs "string-fill!"
scmFill _       = return$ tooFewArgs  "string-fill!"

------

isString :: ScmValue -> Bool
isString (ScmString  _) = True
isString (ScmMString _) = True
isString _              = False

strToList :: ScmValue -> IO String
strToList (ScmString  s) = return (elems s)
strToList (ScmMString s) = getElems s
strToList _              = error "strToList :: the impossible happened"

tryToLen :: String -> Integer -> (Int -> IO a) -> IO (ErrOr a)
tryToLen = tryRange 0 (toInteger (maxBound :: Int))

tryToIdx :: String -> IO Int -> Integer -> (Int -> IO a) -> IO (ErrOr a)
tryToIdx s mlen i f = mlen >>= \len -> tryRange 0 (toInteger len - 1) s i f

tryRange :: Integer -> Integer -> String -> Integer -> (Int -> IO a)
         -> IO (ErrOr a)
tryRange a b s i f = if i >= a && i <= b
                        then Right <$> f (fromInteger i)
                        else return.Left $ "Out-of-range integer to " ++ s

fromChar :: String -> ScmValue -> ErrOr Char
fromChar _ (ScmChar c) = Right c
fromChar s _           = notChar s

sLen :: UArray Int Char -> IO Int
sLen = return . (+1) . snd . bounds

msLen :: IOUArray Int Char -> IO Int
msLen = fmap ((+1) . snd) . getBounds
