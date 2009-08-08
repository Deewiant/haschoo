-- File created: 2009-07-19 18:41:22

{-# LANGUAGE FlexibleContexts #-}

module Haschoo.Evaluator.Utils where

import Control.Monad.Error (MonadError, throwError)

tooFewArgs, tooManyArgs, immutable,
 notList, notInt, notChar, notProcedure, notString
 :: MonadError String m => String -> m a
tooFewArgs   = throwError . ("Too few arguments to " ++)
tooManyArgs  = throwError . ("Too many arguments to " ++)
immutable    = throwError . ("Immutable argument to " ++)
notList      = throwError . ("Nonlist argument to "++)
notInt       = throwError . ("Noninteger argument to " ++)
notChar      = throwError . ("Noncharacter argument to " ++)
notProcedure = throwError . ("Nonprocedural argument to "++)
notString    = throwError . ("Nonstring argument to " ++)
