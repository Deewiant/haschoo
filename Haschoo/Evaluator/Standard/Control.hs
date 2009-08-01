-- File created: 2009-07-22 21:05:24

{-# LANGUAGE ScopedTypeVariables #-}

module Haschoo.Evaluator.Standard.Control (procedures) where

import Data.List           (transpose, genericLength)
import Data.Number.Natural (Natural)

import Haschoo.Types           (ScmValue(..), pairToList, listToPair)
import Haschoo.Utils           (ErrOr, initLast, (.:))
import Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs, notList)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b))
   [ ("procedure?",  return . fmap ScmBool . scmIsProcedure)
   , ("apply",       scmApply)
   , ("map",         scmMap)
   , ("for-each",    scmForEach)
   ]

--- procedure?

scmIsProcedure :: [ScmValue] -> ErrOr Bool
scmIsProcedure [ScmFunc _ _] = Right True
scmIsProcedure [_]           = Right False
scmIsProcedure []            = tooFewArgs  "procedure?"
scmIsProcedure _             = tooManyArgs "procedure?"

--- apply

scmApply :: [ScmValue] -> IO (ErrOr ScmValue)
scmApply (ScmFunc _ f : args@(_:_)) =
   case initLast args of
        (xs, ScmList ys)       -> f (xs ++ ys)
        (xs, ys@(ScmPair _ _)) -> do
           ys' <- pairToList ys
           case ys' of
                Right (ScmList l) -> f (xs ++ l)
                _                 -> return$ notList "apply"

        (_, _) -> return$ notList "apply"

scmApply (_:_) = return$ notProcedure "apply"
scmApply _     = return$ tooFewArgs   "apply"

--- map for-each

scmMap, scmForEach :: [ScmValue] -> IO (ErrOr ScmValue)
scmMap (ScmFunc _ f : args@(_:_)) =
   fmap (fmap ScmList) $ scmIterate "map" [] (:) f args

scmMap (_:_) = return$ notProcedure "map"
scmMap _     = return$ tooFewArgs   "map"

scmForEach (ScmFunc _ f : args@(_:_)) = do
   err <- scmIterate "for-each" () (\_ _ -> ()) f args
   return$ either Left (const $ Right ScmVoid) err

scmForEach (_:_) = return$ notProcedure "for-each"
scmForEach _     = return$ tooFewArgs   "for-each"

scmIterate :: forall a. String -> a -> (ScmValue -> a -> a)
           -> ([ScmValue] -> IO (ErrOr ScmValue))
           -> [ScmValue]
           -> IO (ErrOr a)
scmIterate s end fuse f args = do
   args' <- prepArgs s args
   case args' of
        Right as -> go (genericLength.head $ as) as
        Left err -> return$ Left err
 where
   go :: Natural -> [[ScmValue]] -> IO (ErrOr a)
   go _   []     = return (Right end)
   go len (a:as) = if genericLength a == len
                      then do
                         fa <- f a
                         case fa of
                              Right a' -> do
                                 bs <- go len as
                                 case bs of
                                      Right bs' -> return.Right $ fuse a' bs'
                                      Left err  -> return.Left  $ err
                              Left err -> return.Left $ err

                      else return.Left $ s ++ " :: unmatching list lengths"

prepArgs :: String -> [ScmValue] -> IO (ErrOr [[ScmValue]])
prepArgs s = fmap (fmap transpose . sequence) .: mapM $ \a ->
      case a of
           ScmList xs       -> return.Right $ xs
           xs@(ScmPair _ _) -> do
              ys <- pairToList xs
              return$ case ys of
                           Right (ScmList l) -> Right l
                           _                 -> notList s
           _ -> return$ notList s

--- Utils

notProcedure :: String -> ErrOr a
notProcedure = fail . ("Nonprocedural argument to "++)
