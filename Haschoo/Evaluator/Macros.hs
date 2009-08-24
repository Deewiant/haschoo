-- File created: 2009-07-26 16:12:14

{-# LANGUAGE PatternGuards #-}

module Haschoo.Evaluator.Macros (mkMacro) where

import Control.Applicative         ((<$>))
import Control.Arrow               (first)
import Control.Monad               (liftM2, when)
import Control.Monad.Error         (throwError)
import Control.Monad.Loops         (andM, allM, firstM, untilM)
import Control.Monad.State.Strict  (State, runState, put, get, modify)
import Control.Monad.Trans         (MonadIO, lift, liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Array.IArray           (elems)
import Data.IORef                  (IORef)
import Data.List                   (find)
import Data.Maybe                  (catMaybes, isNothing)
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
                                              , isAggregate, toScmVector)
import Haschoo.Utils                          ( fst3, initLast2Maybe
                                              , ptrEq, eqWithM, ErrOr)
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
              case replaceVars (map fst ls) frees replacements v of
                   Left s -> throwError s
                   Right replaced ->
                      return ( simplifyList replaced
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

   -- E.g. pl = (x y z) and ys = [(1 2 3),(4 5 6)]
   --
   -- Bind x to [1,4], y to [2,5], z to [3,6]
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

             -- From then on it's quite trivial.
             --
             -- Just be sure to add an initial empty match for each identifier.
             -- This ensures that patterns such as ((x) ...), when matched
             -- against (), result in any x in the output being replaced with
             -- nothing, instead of them not being seen as matches.
             matched = [(i, [])  | ScmIdentifier i <- lpat]
                    ++ [(i, [x]) | (ScmIdentifier i, x) <- paired]

          in tell . PM . TM.map Left $ TM.fromListWith (flip (++)) matched

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

replaceVars :: [String] -> [String] -> PatternMatches
            -> ScmValue -> ErrOr [ScmValue]
replaceVars _ _ (PM replacements) (ScmIdentifier i)
   | Just r <- TM.lookup i replacements = case r of
                                               Right r' -> return [r']
                                               Left  rs -> return rs
   | i == "..." = return []

replaceVars lits frees rs (ScmList   l) =
   (:[]) . ScmList     <$> replaceInAggregate lits frees rs l
replaceVars lits frees rs (ScmVector v) =
   (:[]) . toScmVector <$> replaceInAggregate lits frees rs (elems v)

replaceVars lits frees rs (ScmDottedList l x) =
   (:[]) <$> liftM2 ScmDottedList
                    (replaceInAggregate lits frees rs l)
                    (simplifyList <$> replaceVars lits frees rs x)

replaceVars _ _ _ v = return [v]

replaceInAggregate :: [String] -> [String] -> PatternMatches
                   -> [ScmValue] -> ErrOr [ScmValue]
replaceInAggregate lits frees rs (ag : ScmIdentifier "..." : vs)
   | isAggregate ag =
      liftM2 (++) (replaceInEllipticAggregate lits frees rs ag)
                  (replaceInAggregate lits frees rs vs)

replaceInAggregate lits frees rs (v:vs) =
   liftM2 (++) (replaceVars lits frees rs v)
               (replaceInAggregate lits frees rs vs)

replaceInAggregate _ _ _ [] = return []

-- "Pattern variables that occur in subpatterns followed by one or more
-- instances of the identifier ... are allowed only in subtemplates that are
-- followed by as many instances of ....
--
-- They [the subtemplates] are replaced in the output by all of the subforms
-- they [the subpatterns] match in the input, distributed as indicated [in the
-- template, by the positions of the pattern variables]."
--
-- With square-bracketed stuff added to make some sense of the last sentence
-- there.
--
-- E.g. pattern (_ (#(a b) ...)) and corresponding template ((a b) ...).
--
-- Pattern variables a and b are in a subpattern followed by one ellipsis. We
-- also have a subtemplate followed by as many ellipses, and a and b are found
-- nowhere else, so it's valid. Calling this with (_ (#(1 2) #(3 4))) we get
-- the subtemplate '(a b) ...' replaced by '(1 2) (3 4)'.
replaceInEllipticAggregate :: [String] -> [String] -> PatternMatches
                           -> ScmValue -> ErrOr [ScmValue]
replaceInEllipticAggregate lits frees pms val =
   let (replaced, (replacementsLeft,_)) =
          flip runState (TM.empty, pms) $
             (go val `untilM`
                do rsLeft <- fst <$> get
                   return (TM.null rsLeft || anyT (==0) rsLeft))

    in if allT (==0) replacementsLeft
          then return$ catMaybes replaced
          else -- For example:
               --   Pattern has (a ...) (b ...)
               --   Template has ((a b) ...)
               --   (a ...) was (1 2) and (b ...) was (3)
               --   The first (1 3) is fine, but the next (2 ??) isn't
               throwError "Inconsistent match counts for elliptic variables"
 where
   go :: ScmValue -> State (TrieMap Char Int, PatternMatches) (Maybe ScmValue)
   go (ScmIdentifier i)
      | i `elem` frees || i `elem` lits = return Nothing
      | otherwise = do
         (replacementsLeft, PM pm) <- get
         let (v, pm') =
                TM.updateLookup (Just . either (Left . drop 1) Right) i pm
         case v of
              Just (Right r')    -> return (Just r')
              Just (Left [])     -> do modify (first $ TM.insert i 0)
                                       return Nothing
              Just (Left (r:rs)) -> do
                 let rsLeft' =
                        TM.alter' (Just . maybe (length rs) (subtract 1))
                                  i replacementsLeft
                 put (rsLeft', PM pm')
                 return (Just r)

              -- Blank match: e.g. pattern (a ...) matched ()
              Nothing -> return Nothing

   go (ScmList   l) = fmap ScmList     . sequence <$> mapM go l
   go (ScmVector v) = fmap toScmVector . sequence <$> mapM go (elems v)

   go (ScmDottedList l x) = do
      l' <- sequence <$> mapM go l
      x' <- go x
      return (liftM2 ScmDottedList l' x')

   go x = return (Just x)

   anyT f = TM.foldr ((||).f) False
   allT f = TM.foldr ((&&).f) True

simplifyList :: [ScmValue] -> ScmValue
simplifyList [v] = v
simplifyList vs  = ScmList vs
