-- File created: 2009-07-11 21:47:19

-- ScmValue depends on Haschoo
-- Haschoo  depends on Context
-- Context  depends on ScmValue
--
-- Thus they all have to be in the same module, to avoid circular dependencies.
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Haschoo.Types
   ( Haschoo, runHaschoo
   , ScmValue(..), isTrue, pairToList
   , Context(..), mkContext, addToContext, contextLookup, contextSize
   , scmShow
   ) where

import Control.Applicative             ((<$>))
import Control.Monad                   (liftM2)
import Control.Monad.Error             (ErrorT, MonadError, runErrorT)
import Control.Monad.State.Strict      (StateT, MonadState, evalStateT)
import Control.Monad.Trans             (MonadIO)
import Data.Complex                    (Complex((:+)))
import Data.IORef                      (IORef, readIORef)
import Data.IntMap                     (IntMap)
import Data.ListTrie.Patricia.Map.Enum (TrieMap)
import Data.Maybe                      (fromMaybe)
import Data.Ratio                      (numerator, denominator)
import System.IO.Unsafe                (unsafeInterleaveIO)
import Text.Show.Functions             ()

import qualified Data.IntMap as IM
import qualified Data.ListTrie.Patricia.Map.Enum as TM

import Haschoo.Utils (ErrOr, swap, showScmList)

-- Our main monad
newtype Haschoo a = Haschoo (StateT HaschState (ErrorT String IO) a)
   deriving (Functor, Monad, MonadIO, MonadState HaschState, MonadError String)

runHaschoo :: HaschState -> Haschoo a -> IO (ErrOr a)
runHaschoo state (Haschoo h) = runErrorT $ evalStateT h state

type HaschState = [IORef Context]

data ScmValue = ScmPrim       String !([ScmValue] -> Haschoo   ScmValue)
              | ScmFunc       String !([ScmValue] -> IO (ErrOr ScmValue))
              | ScmBool       !Bool
              | ScmChar       !Char
              | ScmString     !String
              | ScmInt        !Integer
              | ScmRat        !Rational
              | ScmReal       !Double
              | ScmComplex    !(Complex Double)
              | ScmIdentifier !String
              | ScmPair       !(IORef ScmValue) !(IORef ScmValue)

              -- These two are only for literal lists, appearing only directly
              -- from the parser
              | ScmList       ![ScmValue]
              | ScmDottedList ![ScmValue] !ScmValue -- The list is never empty

              -- The "unspecified value" returned by IO procedures and such
              | ScmVoid

isTrue :: ScmValue -> Bool
isTrue (ScmBool False) = False
isTrue _               = True

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

data Context = Context { idMap  :: TrieMap Char Int
                       , valMap :: IntMap ScmValue }

mkContext :: [(String, ScmValue)] -> Context
mkContext namedVals = Context ids vals
 where
   key   = zip [0..]
   vals  = IM.fromList .            key . map snd $ namedVals
   ids   = TM.fromList . map swap . key . map fst $ namedVals

addToContext :: String -> ScmValue -> Context -> Context
addToContext name val (Context ids vals) =
   let newId = fromMaybe 0 $ fmap ((+1) . fst . fst) (IM.maxViewWithKey vals)
    in Context (TM.insert' name newId ids)
               (IM.insert  newId val vals)

contextLookup :: String -> Context -> Maybe (Context, Int)
contextLookup s = liftM2 fmap (,) (TM.lookup s . idMap)

contextSize :: Context -> Int
contextSize = IM.size . valMap

scmShow :: ScmValue -> IO String
scmShow ScmVoid           = return "" -- Has no representation
scmShow (ScmBool b)       = return$ '#' : if b then "t" else "f"
scmShow (ScmPrim s _)     = return s
scmShow (ScmFunc s _)     = return s
scmShow (ScmIdentifier s) = return s
scmShow (ScmList xs)      = showScmList scmShow xs
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

scmShow (ScmString s) = return$ '"' : foldr ((.) . f) id s "\""
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c

scmShow (ScmDottedList a b) = do
   as <- showScmList scmShow a
   bs <- scmShow b
   return$ concat [init as, " . ", bs, ")"]

scmShow (ScmPair car cdr) = do
   a  <- readIORef car
   as <- scmShow a
   ('(':) . (as ++) <$> (unsafeInterleaveIO $ readIORef cdr >>= go)
 where
   go   (ScmList [])        = return ")"
   go x@(ScmList _)         = (' ':) . tail <$> scmShow x
   go x@(ScmDottedList _ _) = (' ':) . tail <$> scmShow x

   go (ScmPair x y) = do
      a <- readIORef x
      as <- scmShow a
      (' ':) . (as ++) <$> (unsafeInterleaveIO $ readIORef y >>= go)

   go x = (" . "++) . (++")") <$> scmShow x
