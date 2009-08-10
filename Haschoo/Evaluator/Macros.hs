-- File created: 2009-07-26 16:12:14

{-# LANGUAGE PatternGuards #-}

module Haschoo.Evaluator.Macros (mkMacro) where

import Control.Arrow               ((***))
import Control.Monad               (when)
import Control.Monad.Error         (throwError)
import Control.Monad.Loops         (andM, allM, firstM)
import Control.Monad.Trans         (MonadIO, lift, liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Array.IArray           (bounds, elems)
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
                                              , Context
                                              , toScmVector)
import Haschoo.Utils                          ( fst3, initLast2Maybe
                                              , ptrEq, eqWith, eqWithM)
import Haschoo.Evaluator.Eval                 (maybeEval)
import Haschoo.Evaluator.Standard.Equivalence (scmEqual)

-- The Left case is for ellipses
--
-- The list is for ellipses after lists: (x y) ... must bind (x y) as well as x
-- and y!
data PatternMatches =
   PM (TrieMap Char (Either [ScmValue] ScmValue))
      [(ScmValue,[ScmValue])]

-- Takes care to use a right-biased union so that we can overwrite identifier
-- matches with a second call to tell (important for ellipsis handling)
--
-- We also append the old to the new so that, even if there are duplicates (I
-- think that this can happen only in patterns with an ellipsed list inside
-- another ellipsed list, e.g. "(a (b c) ...) ..."), we will simply find the
-- first match first, since we do a linear scan.
instance Monoid PatternMatches where
   mempty                        = PM mempty mempty
   mappend (PM a1 b1) (PM a2 b2) = PM (mappend a2 a1) (mappend b2 b1)

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
        Nothing           -> do tell $ PM (TM.singleton i (Right arg)) []
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
   assignMatches ys (ScmIdentifier i) =
      tell $ PM (TM.singleton i (Left ys)) []

   -- E.g. pl = (x y z) and ys = [(1 2 3),(4 5 6)]
   --
   -- Bind x to [1,4], y to [2,5], z to [3,6]
   --
   -- And (x y z) itself to ys, as well!
   assignMatches ys p =
      case p of
           ScmList       _   -> assignList (\(ScmList       l)   -> l)
           ScmVector     _   -> assignList (\(ScmVector     v)   -> elems v)
           ScmDottedList _ _ -> assignList (\(ScmDottedList l x) -> x:l)
           _                 -> return ()

    where
      assignList toList =
         let -- Continuing the above example, this would give
             -- [(x,1),(x,4),(y,2),(y,5),(z,3),(z,6)]
             lpat    = toList p
             paired  = concatMap (zip lpat . toList) ys

             -- From then on it's quite trivial
             matched = map    ((\(ScmIdentifier i) -> i) *** (:[]))
                     . filter (isIdentifier . fst)
                     $ paired

          in tell $ PM (TM.map Left $ TM.fromListWith (flip (++)) matched)
                       [(p, ys)]

      isIdentifier (ScmIdentifier _) = True
      isIdentifier _                 = False

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
replaceVars (PM replacements _) (ScmIdentifier i)
   | Just r <- TM.lookup i replacements = case r of
                                               Right r' -> [r']
                                               Left  rs -> rs
   | i == "..." = []

replaceVars rs v@(ScmList l) =
   tryDirect rs v $ ScmList (concatMap (replaceVars rs) l)

replaceVars rs v@(ScmDottedList l x) =
   tryDirect rs v $ ScmDottedList (concatMap (replaceVars rs) l)
                                  (simplifyList $ replaceVars rs x)

replaceVars rs v@(ScmVector l) =
   tryDirect rs v $ toScmVector (concatMap (replaceVars rs) (elems l))

replaceVars _ v = [v]

-- For aggregates, try direct matches: if we have a pattern (x y) ..., and the
-- use is (x y) ..., replace (x y) instead of x and y individually
tryDirect :: PatternMatches -> ScmValue -> ScmValue -> [ScmValue]
tryDirect (PM _ replacements) v alternative =
   maybe [alternative] snd (find (eq v . fst) replacements)
 where
   -- We know that identifiers are unique among all patterns, so equality among
   -- the identifiers is sufficient
   eq (ScmIdentifier a) (ScmIdentifier b) = a == b
   eq (ScmList       a) (ScmList       b) = eqWith eq a b
   eq (ScmVector     a) (ScmVector     b) =
      len a == len b && eqWith eq (elems a) (elems b)

   eq (ScmDottedList as a) (ScmDottedList bs b) = eq a b && eqWith eq as bs

   eq _ _ = False

   len = (+1) . snd . bounds

simplifyList :: [ScmValue] -> ScmValue
simplifyList [v] = v
simplifyList vs  = ScmList vs
