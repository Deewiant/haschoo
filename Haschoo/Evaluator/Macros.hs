-- File created: 2009-07-26 16:12:14

{-# LANGUAGE PatternGuards #-}

module Haschoo.Evaluator.Macros (mkMacro) where

import Control.Monad               (when)
import Control.Monad.Error         (throwError)
import Control.Monad.Loops         (andM, allM, firstM)
import Control.Monad.Trans         (MonadIO, lift, liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Array.IArray           (elems)
import Data.IORef                  (IORef)
import Data.List                   (find)
import Data.Maybe                  (isNothing)
import Data.Monoid                 (Monoid(mempty, mappend))

import qualified Data.ListTrie.Patricia.Map.Enum as TM
import Data.ListTrie.Patricia.Map.Enum (TrieMap)

import Haschoo.Types                          ( Haschoo
                                              , ScmValue
                                                   ( ScmMacro, ScmIdentifier
                                                   , ScmList, ScmDottedList
                                                   , ScmVector)
                                              , MacroCall(..)
                                              , Context)
import Haschoo.Utils                          ( fst3, initLast2Maybe
                                              , ptrEq, eqWithM)
import Haschoo.Evaluator.Eval                 (maybeEval)
import Haschoo.Evaluator.Standard.Equivalence (scmEqual)

-- The Left case is for ellipses
newtype PatternMatches = PM (TrieMap Char (Either [ScmValue] ScmValue))

-- Right-biased union instead of the default left-biased: so we can overwrite
-- keys in a Writer with a second call to tell
instance Monoid PatternMatches where
   mempty                = PM mempty
   mappend (PM a) (PM b) = PM (TM.union b a)

type Lits = [(String, Maybe ScmValue)]

mkMacro :: [IORef Context] -> String -> [(ScmValue, ScmValue, [String])]
        -> Lits -> ScmValue
mkMacro ctx name pats ls = ScmMacro name f
 where
   f args = do
      (matching,replacements) <-
         runWriterT $ firstM (match ls args . fst3) pats

      case matching of
           Nothing          -> throwError ("Invalid use of macro " ++ name)
           Just (_,v,frees) ->
              return ( simplifyList $ replaceVars replacements v
                     , TM.fromList (zip frees (repeat ctx)))

-- Turning MCDotted to ScmDottedList here means that the list in the
-- ScmDottedList may actually be empty, which can't happen normally
match :: Lits -> MacroCall -> ScmValue -> WriterT PatternMatches Haschoo Bool
match lits (MCList   xs)   = match1 lits (ScmList xs)
match lits (MCDotted xs x) = match1 lits (ScmDottedList xs x)

-- Paraphrasing R5RS 4.3.2:
--
-- "... formally, an input form F matches a pattern P iff:" ...
match1 :: Lits -> ScmValue -> ScmValue -> WriterT PatternMatches Haschoo Bool
match1 lits arg (ScmIdentifier i) =
   case find ((== i).fst) lits of
        -- ... "P is a non-literal identifier" ...
        Nothing           -> do tell (PM $ TM.singleton i (Right arg))
                                return True
        Just (_, binding) ->
           case arg of
                -- ... "P is a literal identifier and F is an identifier with
                -- the same binding, or the two identifiers are equal and both
                -- have no lexical binding" ...
                x@(ScmIdentifier i2) -> do
                   xb <- lift $ maybeEval x
                   case binding of
                        Nothing -> return (isNothing xb && i == i2)
                        Just pb -> maybe (return False) (liftIO . ptrEq pb) xb

                _  -> return False

match1 lits (ScmList args) (ScmList ps) =
   case initLast2Maybe ps of
        -- ... "P is of the form (P1 ... Pn Pm <ellipsis>) where <ellipsis> is
        -- the identifier ... and F is a list of at least n forms, the first n
        -- of which match P1 through Pn and each remaining element matches Pm"
        -- ...
        Just (ps', p, ScmIdentifier "...") ->
           let (xs, ys) = splitAt (length ps') args
            in do m <- andM [ allMatch lits xs ps'
                            , allM (flip (match1 lits) p) ys]
                  when m (assignMatches ys p)
                  return m

        -- ... "P is a list (P1 ... Pn) and F is a list of n forms that match
        -- P1 through Pn" ...
        _ -> allMatch lits args ps

 where
   assignMatches ys (ScmIdentifier i) = tell (PM $ TM.singleton i (Left ys))
   assignMatches _  _                 = return ()

-- ... "P is an improper list (P1 ... Pn . Pm)" ...
match1 lits args (ScmDottedList ps p) =
   case args of
        -- ... "and F is a list or improper list of n or more forms that match
        -- P1 through Pn and whose nth cdr matches Pm" ...
        ScmList as -> let (xs, ys) = splitAt (length ps) as
                       in andM [ allMatch lits xs ps
                               , match1 lits (ScmList ys) p]

        ScmDottedList as a -> andM [allMatch lits as ps, match1 lits a p]
        _                  -> return False

-- ... "P is a vector of the form #(P1 ... Pn) and F is a vector of n forms
-- that match P1 through Pn" ...
--
-- ... "P is of the form #(P1 ... Pn Pm <ellipsis>) where <ellipsis> is the
-- identifier ... and F is a vector of n or more forms, the first n of which
-- match P1 through Pn and each remaining element matches Pm" ...
--
-- Just forward to the list one, the rules are identical
match1 lits (ScmVector args) (ScmVector ps) =
   match1 lits (ScmList $ elems args) (ScmList $ elems ps)

-- ... "P is a datum and F is equal to P in the sense of the equal? procedure".
match1 _ arg p = liftIO $ scmEqual arg p

allMatch :: Lits -> [ScmValue] -> [ScmValue]
         -> WriterT PatternMatches Haschoo Bool
allMatch = eqWithM . match1

replaceVars :: PatternMatches -> ScmValue -> [ScmValue]
replaceVars (PM replacements) (ScmIdentifier i)
   | Just r <- TM.lookup i replacements = case r of
                                               Right r' -> [r']
                                               Left  rs -> rs
   | i == "..." = []

replaceVars rs (ScmList l) = [ScmList (concatMap (replaceVars rs) l)]

replaceVars rs (ScmDottedList l x) =
   [ScmDottedList (concatMap (replaceVars rs) l)
                  (simplifyList $ replaceVars rs x)]

replaceVars _ v = [v]

simplifyList :: [ScmValue] -> ScmValue
simplifyList [v] = v
simplifyList vs  = ScmList vs
