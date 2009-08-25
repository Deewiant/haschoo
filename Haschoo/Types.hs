-- File created: 2009-07-11 21:47:19

-- ScmValue depends on Haschoo
-- Haschoo  depends on Context
-- Context  depends on ScmValue
--
-- Thus they all have to be in the same module, to avoid circular dependencies.
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Haschoo.Types
   ( Haschoo, runHaschoo, withHaschoo
   , ScmValue(..), MacroCall(..)
   , isTrue, isAggregate
   , toScmString, toScmMString
   , toScmVector
   , pairToList, listToPair
   , Context, mkContext, addToContext, contextLookup, contextSize
   , scmShow, scmShowWith
   ) where

import Control.Applicative        ((<$>))
import Control.Arrow              (second)
import Control.Monad.Error        (ErrorT, MonadError, runErrorT)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT)
import Control.Monad.Trans        (MonadIO, liftIO)
import Data.Array.IO              (IOArray, IOUArray, newListArray, getElems)
import Data.Array.Unboxed         (Array, UArray, elems, listArray)
import Data.Complex               (Complex((:+)))
import Data.IORef                 (IORef, readIORef, newIORef)
import Data.List                  (intercalate)
import Data.Ratio                 (numerator, denominator)
import System.IO                  (Handle)
import System.IO.Unsafe           (unsafeInterleaveIO)
import Text.Show.Functions        ()

import qualified Data.ListTrie.Patricia.Map.Enum as TM
import Data.ListTrie.Patricia.Map.Enum (TrieMap)

import Haschoo.Utils (ErrOr, lazyMapM)

-- Our main monad
newtype Haschoo a = Haschoo (StateT HaschState (ErrorT String IO) a)
   deriving (Functor, Monad, MonadIO, MonadState HaschState, MonadError String)

runHaschoo :: HaschState -> Haschoo a -> IO (ErrOr a)
runHaschoo state (Haschoo h) = runErrorT $ evalStateT h state

-- Like 'local' of the Reader monad
withHaschoo :: HaschState -> Haschoo a -> Haschoo a
withHaschoo state h = either fail return =<< liftIO (runHaschoo state h)

type HaschState = [IORef Context]

data ScmValue = ScmPrim  String !([ScmValue] -> Haschoo   ScmValue)
              | ScmFunc  String !([ScmValue] -> IO (ErrOr ScmValue))

              | ScmMacro String
                   !(MacroCall ->
                        Haschoo (ScmValue, TrieMap Char [IORef Context]))

              | ScmBool       !Bool
              | ScmChar       !Char
              | ScmInt        !Integer
              | ScmRat        !Rational
              | ScmReal       !Double
              | ScmComplex    !(Complex Double)
              | ScmIdentifier !String
              | ScmPair       !(IORef ScmValue) !(IORef ScmValue)
              | ScmString     !(  UArray Int Char)
              | ScmMString    !(IOUArray Int Char)
              | ScmVector     !(   Array Int ScmValue)
              | ScmMVector    !( IOArray Int ScmValue)

              | ScmSyntax { patterns :: ![(ScmValue, ScmValue, [String])]
                          , literals :: ![(String, Maybe ScmValue)] }

              -- These two are only for literal lists, appearing only directly
              -- from the parser
              | ScmList       ![ScmValue]
              | ScmDottedList ![ScmValue] !ScmValue -- The list is never empty

              -- The "unspecified value" returned by IO procedures and such
              | ScmVoid

              | ScmInput  !Handle
              | ScmOutput !Handle
              | ScmEOF

              -- The "environment-specifier" used by eval-related procedures
              | ScmContext ![Context]

-- The form of the macro usage:
--
-- (macro foo bar), (macro foo . bar), or #(macro foo bar)
--
-- The macro keyword is not included regardless of constructor
data MacroCall = MCList   ![ScmValue]
               | MCDotted ![ScmValue] !ScmValue

isTrue, isAggregate :: ScmValue -> Bool
isTrue (ScmBool False) = False
isTrue _               = True

isAggregate (ScmList       _)   = True
isAggregate (ScmDottedList _ _) = True
isAggregate (ScmVector     _)   = True
isAggregate (ScmMVector    _)   = True
isAggregate (ScmPair       _ _) = True
isAggregate _                   = False

-- From ScmPair to Left . ScmDottedList or Right . ScmList
pairToList :: ScmValue -> IO (Either ScmValue ScmValue)
pairToList = go []
 where
   go xs (ScmList       ys)   =
      return . Right $ ScmList       (reverse xs ++ ys)
   go xs (ScmDottedList ys z) =
      return . Left  $ ScmDottedList (reverse xs ++ ys) z

   go xs (ScmPair a b) = do
      a' <- readIORef a
      b' <- readIORef b
      go (a':xs) b'

   go xs v = return . Left $ ScmDottedList (reverse xs) v

-- ScmList -> ScmPair, returns the tail pointer if it wasn't empty
listToPair :: [ScmValue] -> IO (ScmValue, Maybe (IORef ScmValue))
listToPair []     = return (ScmList [], Nothing)
listToPair (x:xs) = second Just <$> go x xs
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

toScmString :: String -> ScmValue
toScmString s = ScmString $ listArray (0, length s - 1) s

toScmMString :: String -> IO ScmValue
toScmMString s = ScmMString <$> newListArray (0, length s - 1) s

toScmVector :: [ScmValue] -> ScmValue
toScmVector v = ScmVector $ listArray (0, length v - 1) v

newtype Context = Context (TrieMap Char ScmValue)

mkContext :: [(String, ScmValue)] -> Context
mkContext = Context . TM.fromList

addToContext :: String -> ScmValue -> Context -> Context
addToContext name val (Context c) = Context (TM.insert' name val c)

contextLookup :: String -> Context -> Maybe ScmValue
contextLookup s (Context c) = TM.lookup s c

contextSize :: Context -> Int
contextSize (Context c) = TM.size' c

scmShow :: ScmValue -> IO String
scmShow ScmVoid           = return "" -- Has no representation
scmShow (ScmBool b)       = return$ '#' : if b then "t" else "f"
scmShow (ScmPrim s _)     = return s
scmShow (ScmFunc s _)     = return s
scmShow (ScmMacro s _)    = return s
scmShow (ScmIdentifier s) = return s
scmShow (ScmInt n)        = return$ show n
scmShow (ScmReal n)
   | isNaN      n = return "+nan.#"
   | isInfinite n = return$ (if n < 0 then '-' else '+') : "inf.#"
   | otherwise    = return$ show n
scmShow (ScmRat n) =
   return$ concat [show (numerator n), "/", show (denominator n)]

scmShow (ScmComplex (a :+ b)) = do
   bs <- scmShow (ScmReal b)
   as <- scmShow (ScmReal a)
   return$ concat [ as
                  , if head bs `elem` "-+" then [] else "+"
                  , bs
                  , "i" ]

scmShow (ScmChar c) | c == ' '  = return  "#\\space"
                    | c == '\n' = return  "#\\newline"
                    | otherwise = return$ "#\\" ++ [c]

scmShow (ScmSyntax  _ _) = return "<syntax rules>"
scmShow (ScmContext _)   = return "<environment specifier>"
scmShow ScmEOF           = return "<end of file object>"
scmShow (ScmInput  _)    = return "<input port>"
scmShow (ScmOutput _)    = return "<output port>"

scmShow (ScmString  s) = return$ showScmString (elems s)
scmShow (ScmMString s) = showScmString <$> getElems s

scmShow x | isAggregate x = scmShowWith scmShow x
          | otherwise     = error "scmShow :: the impossible happened"

scmShowWith :: (ScmValue -> IO String) -> ScmValue -> IO String
scmShowWith f (ScmList xs)        = showScmList f xs
scmShowWith f (ScmDottedList a b) = do
   as <- showScmList f a
   bs <- f b
   return$ concat [init as, " . ", bs, ")"]

scmShowWith f (ScmVector  v) = ('#':) <$>  showScmList f       (elems v)
scmShowWith f (ScmMVector v) = ('#':) <$> (showScmList f =<< getElems v)

scmShowWith f (ScmPair car cdr) = do
   a  <- readIORef car
   as <- f a
   ('(':) . (as ++) <$> unsafeInterleaveIO (readIORef cdr >>= go)
 where
   go   (ScmList [])        = return ")"
   go x@(ScmList _)         = (' ':) . tail <$> f x
   go x@(ScmDottedList _ _) = (' ':) . tail <$> f x

   go (ScmPair x y) = do
      a <- readIORef x
      as <- f a
      (' ':) . (as ++) <$> unsafeInterleaveIO (readIORef y >>= go)

   go x = (" . "++) . (++")") <$> f x

scmShowWith _ _ = error "scmShowWith :: the impossible happened"

showScmString :: String -> String
showScmString s = '"' : foldr ((.) . f) id s "\""
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c

showScmList :: (a -> IO String) -> [a] -> IO String
showScmList f xs =
   ("("++) . (++")") . intercalate " " <$> lazyMapM f xs
