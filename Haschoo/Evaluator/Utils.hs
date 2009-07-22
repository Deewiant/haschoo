-- File created: 2009-07-19 18:41:22

{-# LANGUAGE FlexibleContexts #-}

module Haschoo.Evaluator.Utils where

import Control.Monad.Error (MonadError, throwError)

tooFewArgs, tooManyArgs, notList :: MonadError String m => String -> m a
tooFewArgs  = throwError . ("Too few arguments to " ++)
tooManyArgs = throwError . ("Too many arguments to " ++)

notList = throwError . ("Nonlist argument to "++)
