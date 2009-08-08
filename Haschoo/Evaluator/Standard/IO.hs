-- File created: 2009-07-20 13:30:25

module Haschoo.Evaluator.Standard.IO (procedures) where

import Control.Arrow     ((&&&))
import Control.Monad     (liftM2)
import Data.Array.IArray (elems)
import Data.Array.MArray (getElems)
import System.IO         ( Handle, IOMode(..), stdin, stdout
                         , openFile, hClose, hIsEOF
                         , hGetChar, hLookAhead, hReady
                         , hPutStr, hPutStrLn, hPutChar)

import Text.ParserCombinators.Poly.Plain (runParser)

import Haschoo.Parser          (value)
import Haschoo.Types           (ScmValue(..), scmShow, scmShowWith)
import Haschoo.Utils           (ErrOr, ($<), (.:))
import Haschoo.Evaluator.Utils ( tooFewArgs, tooManyArgs
                               , notProcedure, notString, notChar)

import Haschoo.Evaluator.Standard.Strings (isString, strToList)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a b))
   [ ("port?",                return .  fmap ScmBool .  scmIsPort)
   , "input-port?"  $< id &&& return .: fmap ScmBool .: scmIsInputPort
   , "output-port?" $< id &&& return .: fmap ScmBool .: scmIsOutputPort

   , ("eof-object?", return . fmap ScmBool . scmIsEof)

   , "call-with-input-file"  $< id &&& scmCallWithFile  ReadMode ScmInput
   , "call-with-output-file" $< id &&& scmCallWithFile WriteMode ScmOutput

   , "current-input-port"  $< id &&& flip constant (return $ ScmInput  stdin)
   , "current-output-port" $< id &&& flip constant (return $ ScmOutput stdout)

   , "open-input-file"  $< id &&& scmOpenFile  ReadMode ScmInput
   , "open-output-file" $< id &&& scmOpenFile WriteMode ScmOutput

   , ("close-input-port",  scmCloseInputPort)
   , ("close-output-port", scmCloseOutputPort)

   , "read"       $< id &&& \s -> withInput 0 s (constant s . scmRead)
   , "read-char"  $< id &&& \s -> withInput 0 s (constant s . scmReadChar)
   , "peek-char"  $< id &&& \s -> withInput 0 s (constant s . scmPeekChar)
   , "char-ready" $< id &&& \s -> withInput 0 s (constant s . scmCharReady)

   , "write"      $< id &&& \s -> withOutput 1 s scmWrite
   , "display"    $< id &&& \s -> withOutput 1 s scmDisplay
   , "newline"    $< id &&& \s -> withOutput 0 s (constant s . scmNewline)
   , "write-char" $< id &&& \s -> withOutput 1 s scmWriteChar
   ]

scmIsInputPort, scmIsOutputPort :: String -> [ScmValue] -> ErrOr Bool
scmIsInputPort  _ [ScmInput _]  = Right True
scmIsInputPort  _ [_]           = Right False
scmIsInputPort  s []            = tooFewArgs  s
scmIsInputPort  s _             = tooManyArgs s
scmIsOutputPort _ [ScmOutput _] = Right True
scmIsOutputPort _ [_]           = Right False
scmIsOutputPort s []            = tooFewArgs  s
scmIsOutputPort s _             = tooManyArgs s

scmIsPort :: [ScmValue] -> ErrOr Bool
scmIsPort x =
   liftM2 (||) (scmIsInputPort "port?" x) (scmIsOutputPort "port?" x)

scmIsEof :: [ScmValue] -> ErrOr Bool
scmIsEof [ScmEOF] = Right True
scmIsEof [_]      = Right False
scmIsEof []       = tooFewArgs "eof-object?"
scmIsEof _        = tooManyArgs "eof-object?"

scmCallWithFile :: IOMode -> (Handle -> ScmValue) -> String
                -> [ScmValue] -> IO (ErrOr ScmValue)
scmCallWithFile mode toPort _ [s, ScmFunc _ f] | isString s = do
   path <- strToList s
   f . return . toPort =<< openFile path mode

scmCallWithFile _ _ s [_, ScmFunc _ _] = return$ notString    s
scmCallWithFile _ _ s [_,_]            = return$ notProcedure s
scmCallWithFile _ _ s (_:_:_)          = return$ tooManyArgs  s
scmCallWithFile _ _ s _                = return$ tooFewArgs   s

scmOpenFile :: IOMode -> (Handle -> ScmValue) -> String
            -> [ScmValue] -> IO (ErrOr ScmValue)
scmOpenFile mode toPort _ [s] | isString s = do
   path <- strToList s
   fmap (Right . toPort) (openFile path mode)

scmOpenFile _ _ s [_] = return$ notString   s
scmOpenFile _ _ s []  = return$ tooFewArgs  s
scmOpenFile _ _ s _   = return$ tooManyArgs s

scmCloseInputPort, scmCloseOutputPort :: [ScmValue] -> IO (ErrOr ScmValue)
scmCloseInputPort  [ScmInput  h] = hClose h >> return (Right ScmVoid)
scmCloseInputPort  [_]           = return$ notInput    "close-input-port"
scmCloseInputPort  []            = return$ tooFewArgs  "close-input-port"
scmCloseInputPort  _             = return$ tooManyArgs "close-input-port"
scmCloseOutputPort [ScmOutput h] = hClose h >> return (Right ScmVoid)
scmCloseOutputPort [_]           = return$ notOutput   "close-output-port"
scmCloseOutputPort []            = return$ tooFewArgs  "close-output-port"
scmCloseOutputPort _             = return$ tooManyArgs "close-output-port"

-- Meh, this is a pain. The parser is pure so we'd basically need to give it
-- the result of hGetContents; but that breaks any further input ("semi-closes"
-- the handle). So just read a char at a time until we get a successful parse.
--
-- The spec doesn't say what to do if we get nonsense like anything starting
-- with ")", so we choose to loop forever (or until EOF).
--
-- O(n^2 + np) where n is the length of the input and p the time it takes for a
-- parse.
scmRead :: Handle -> IO ScmValue
scmRead hdl = go []
 where
   go s = do
      eof <- hIsEOF hdl
      if eof
         then return ScmEOF
         else do
            c <- hGetChar hdl
            let s' = s ++ [c]
            either (const $ go s') return (fst $ runParser value s')

scmReadChar, scmPeekChar, scmCharReady :: Handle -> IO ScmValue
scmReadChar  = fmap ScmChar . hGetChar
scmPeekChar  = fmap ScmChar . hLookAhead
scmCharReady = fmap ScmBool . hReady

scmWrite, scmDisplay :: Handle -> [ScmValue] -> IO (ErrOr ScmValue)
scmWrite   = scmPrint scmShow "write"
scmDisplay = scmPrint f "display"
 where
   f (ScmString  s) = return (elems s)
   f (ScmMString s) = getElems s
   f (ScmChar    c) = return [c]
   f x              = scmShowWith f x

scmPrint :: (ScmValue -> IO String) -> String
         -> Handle -> [ScmValue] -> IO (ErrOr ScmValue)
scmPrint f _ h [x] = f x >>= hPutStr h >> return (Right ScmVoid)
scmPrint _ s _ []  = return$ tooFewArgs  s
scmPrint _ s _ _   = return$ tooManyArgs s

scmNewline :: Handle -> IO ScmValue
scmNewline h = hPutStrLn h "" >> return ScmVoid

scmWriteChar :: Handle -> [ScmValue] -> IO (ErrOr ScmValue)
scmWriteChar h [ScmChar c] = hPutChar h c >> return (Right ScmVoid)
scmWriteChar _ [_]         = return$ notChar     "write-char"
scmWriteChar _ []          = return$ tooFewArgs  "write-char"
scmWriteChar _ _           = return$ tooManyArgs "write-char"

-----

notInput, notOutput :: String -> ErrOr a
notInput  = fail . ("Non-input-port argument to " ++)
notOutput = fail . ("Non-output-port argument to " ++)

constant :: String -> IO ScmValue -> [ScmValue] -> IO (ErrOr ScmValue)
constant _ x [] = fmap Right x
constant s _ _  = return$ tooManyArgs s

-- These take advantage of the fact that the port is always the last argument
--
-- withInput does an EOF check as well
withInput, withOutput :: Int
                      -> String
                      -> (Handle -> [ScmValue] -> IO (ErrOr ScmValue))
                      -> [ScmValue] -> IO (ErrOr ScmValue)
withInput idx s f = withPort stdin apply idx s f
 where
   apply (ScmInput h) xs = do
      eof <- hIsEOF h
      if eof
         then return$ Right ScmEOF
         else f h xs
   apply _            _  = return$ notInput s

withOutput idx s f = withPort stdout apply idx s f
 where
   apply (ScmOutput h) = f h
   apply _             = const.return$ notOutput s

withPort :: Handle
         -> (ScmValue -> ([ScmValue] -> IO (ErrOr ScmValue)))
         -> Int
         -> String
         -> (Handle -> [ScmValue] -> IO (ErrOr ScmValue))
         -> [ScmValue] -> IO (ErrOr ScmValue)

withPort defaultHdl apply idx s f args =
   let (xs, p) = splitAt idx args
    in case p of
            [x] -> apply x xs
            []  -> f defaultHdl xs
            _   -> return$ tooManyArgs s
