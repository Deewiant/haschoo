-- File created: 2009-07-11 21:47:19

-- ScmValue depends on Datum and Haschoo
-- Datum    depends on ScmValue
-- Haschoo  depends on Context
-- Context  depends on ScmValue
--
-- Thus they all have to be in the same module, to avoid circular dependencies.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haschoo.Types
   ( Haschoo, runHaschoo
   , Datum(..), ScmValue(..), scmShow, scmShowDatum
   , Context(..), mkContext, contextSize
   ) where

import           Control.Monad.Error        (ErrorT, MonadError, runErrorT)
import           Control.Monad.State.Strict (StateT, MonadState, evalStateT)
import           Control.Monad.Trans        (MonadIO)
import           Data.Complex               (Complex((:+)))
import           Data.IntMap                (IntMap)
import qualified Data.IntMap as IM
import           Data.ListTrie.Patricia.Map.Enum (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Enum as TM
import           Data.Ratio          (numerator, denominator)
import           Text.Show.Functions ()

import Haschoo.Utils (ErrOr, swap, showScmList)

-- Our main monad
newtype Haschoo a = Haschoo (StateT [Context] (ErrorT String IO) a)
   deriving (Functor, Monad, MonadIO, MonadState [Context], MonadError String)

runHaschoo :: [Context] -> Haschoo a -> IO (ErrOr a)
runHaschoo ctx (Haschoo h) = runErrorT $ evalStateT h ctx

data Datum = Evaluated    ScmValue
           | Quoted       Datum
           | QuasiQuoted  Datum
           | UnQuoted     Datum
           | FlatUnQuoted Datum
           | UnevaledId   String
           | UnevaledApp  [Datum]
           | UnevaledVec  [Datum]
           | DottedList   [Datum] Datum
 deriving Show

data ScmValue = ScmPrim    String !([Datum]    -> Haschoo   ScmValue)
              | ScmFunc    String !([ScmValue] -> IO (ErrOr ScmValue))
              | ScmBool    !Bool
              | ScmChar    !Char
              | ScmString  !String
              | ScmInt     !Integer
              | ScmRat     !Rational
              | ScmReal    !Double
              | ScmComplex !(Complex Double)
              | ScmList    ![ScmValue]

              -- The "unspecified value" returned by IO procedures and such
              | ScmVoid
 deriving Show

data Context = Context { idMap  :: TrieMap Char Int
                       , valMap :: IntMap ScmValue }
 deriving Show

mkContext :: [(String, ScmValue)] -> Context
mkContext namedVals = Context ids vals
 where
   key   = zip [0..]
   vals  = IM.fromList .            key . map snd $ namedVals
   ids   = TM.fromList . map swap . key . map fst $ namedVals

contextSize :: Context -> Int
contextSize = IM.size . valMap

scmShow :: ScmValue -> String
scmShow ScmVoid       = "" -- Has no representation
scmShow (ScmBool b)   = '#' : if b then "t" else "f"
scmShow (ScmPrim s _) = s
scmShow (ScmFunc s _) = s
scmShow (ScmList xs)  = showScmList scmShow xs
scmShow (ScmInt n)    = show n
scmShow (ScmReal n)
   | isNaN      n = "+nan.#"
   | isInfinite n = (if n < 0 then '-' else '+') : "inf.#"
   | otherwise    = show n
scmShow (ScmRat n)    =
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

scmShowDatum :: Datum -> String
scmShowDatum (Evaluated v)     = scmShow v
scmShowDatum (Quoted x)        = '\'': scmShowDatum x
scmShowDatum (QuasiQuoted x)   = '`' : scmShowDatum x
scmShowDatum (UnQuoted x)      = ',' : scmShowDatum x
scmShowDatum (FlatUnQuoted x)  = ",@" ++ scmShowDatum x
scmShowDatum (UnevaledId s)    = s
scmShowDatum (UnevaledApp xs)  = showScmList scmShowDatum xs
scmShowDatum (UnevaledVec xs)  = '#' : showScmList scmShowDatum xs
scmShowDatum (DottedList xs x) =
   concat [init (showScmList scmShowDatum xs), " . ", scmShowDatum x, ")"]
