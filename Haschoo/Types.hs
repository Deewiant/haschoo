-- File created: 2009-07-11 21:47:19

-- ScmValue depends on Haschoo
-- Haschoo  depends on Context
-- Context  depends on ScmValue
--
-- Thus they all have to be in the same module, to avoid circular dependencies.
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Haschoo.Types
   ( Haschoo, runHaschoo, withHaschoo
   , ScmValue(..), isTrue
   , Context(..), mkContext, addToContext, contextLookup, contextSize
   , scmShow
   ) where

import Control.Monad                   (liftM2)
import Control.Monad.Error             ( ErrorT, MonadError, runErrorT
                                       , throwError)
import Control.Monad.State.Strict      (StateT, MonadState, evalStateT, get)
import Control.Monad.Trans             (MonadIO, liftIO)
import Data.Complex                    (Complex((:+)))
import Data.IORef                      (IORef)
import Data.IntMap                     (IntMap)
import Data.ListTrie.Patricia.Map.Enum (TrieMap)
import Data.Maybe                      (fromMaybe)
import Data.Ratio                      (numerator, denominator)
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

-- Like 'local' in MonadReader. (Not like withStateT in MonadState, which
-- evidently does something different.)
withHaschoo :: (HaschState -> HaschState) -> Haschoo a -> Haschoo a
withHaschoo f h = do
   state <- get
   x <- liftIO $ runHaschoo (f state) h
   case x of
        Left  s -> throwError s
        Right a -> return a

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
              | ScmPair       !ScmValue !ScmValue

              -- These two are only for literal lists, appearing only directly
              -- from the parser
              | ScmList       ![ScmValue]
              | ScmDottedList ![ScmValue] !ScmValue -- The list is never empty

              -- The "unspecified value" returned by IO procedures and such
              | ScmVoid
 deriving Show

isTrue :: ScmValue -> Bool
isTrue (ScmBool False) = False
isTrue _               = True

data Context = Context { idMap  :: TrieMap Char Int
                       , valMap :: IntMap ScmValue }
 deriving Show

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

---- Show functions

scmShow :: ScmValue -> String
scmShow ScmVoid           = "" -- Has no representation
scmShow (ScmBool b)       = '#' : if b then "t" else "f"
scmShow (ScmPrim s _)     = s
scmShow (ScmFunc s _)     = s
scmShow (ScmIdentifier s) = s
scmShow (ScmList xs)      = showScmList scmShow xs
scmShow (ScmInt n)        = show n
scmShow (ScmReal n)
   | isNaN      n = "+nan.#"
   | isInfinite n = (if n < 0 then '-' else '+') : "inf.#"
   | otherwise    = show n
scmShow (ScmRat n) =
   concat [show (numerator n), "/", show (denominator n)]

scmShow (ScmComplex (a :+ b)) =
   let bs = scmShow (ScmReal b)
    in concat [ scmShow (ScmReal a)
              , if head bs `elem` "-+" then [] else "+"
              , bs
              , "i" ]

scmShow (ScmChar c) | c == ' '  = "#\\space"
                    | c == '\n' = "#\\newline"
                    | otherwise = "#\\" ++ [c]

scmShow (ScmString s) = '"' : foldr ((.) . f) id s "\""
 where
   f c | c == '\\' = showString "\\\\"
       | c == '\"' = showString "\\\""
       | otherwise = showChar c

scmShow (ScmDottedList as b) =
   concat [init (showScmList scmShow as), " . ", scmShow b, ")"]

scmShow (ScmPair car cdr) = '(' : scmShow car ++ go cdr
 where
   go (ScmPair x y) = " " ++ scmShow x ++ go y
   go b             = " . " ++ scmShow b ++ ")"
