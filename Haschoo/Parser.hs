-- File created: 2009-07-11 20:29:49

module Haschoo.Parser (parser) where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Text.ParserCombinators.Poly.Plain

import Haschoo.Datum (Datum(..))
import Haschoo.ScmValue (ScmValue(ScmBool, ScmInt, ScmChar, ScmString))
import Haschoo.Utils    (void)

parser :: Parser Char [Datum]
parser = do
   ds <- many datum
   eof
   return ds

datum :: Parser Char Datum
datum = do
   atmosphere
   quoted <- optional (one '\'' >> commit atmosphere)
   dat    <- oneOf [ident, list, vector, value]
   return$ if isJust quoted
              then Quoted dat
              else dat

value :: Parser Char Datum
value = Evaluated <$> oneOf [bool, number, character, quotedString]

ident :: Parser Char Datum
ident = do
   x <- oneOf [peculiar,ordinary]
   delimiter
   return x
 where
   peculiar = UnevaledId <$> oneOf [return <$> pElem "+-", string "..."]
   ordinary = do
      x  <- pElem initial
      xs <- many (pElem (initial ++ "+-.@" ++ ['0'..'9']))
      return$ UnevaledId (x:xs)
    where
      initial = ['a'..'z'] ++ "!$%&*/:<=>?^_~"

bool :: Parser Char ScmValue
bool = one '#' >> ScmBool . (=='t') <$> (pElem "tf")

-- FIXME: only does base 10 integers
number :: Parser Char ScmValue
number = do
   x  <- satisfy isDigit
   xs <- commit $ manyFinally (satisfy isDigit) delimiter
   return . ScmInt $ read (x:xs)

character :: Parser Char ScmValue
character = do
   string "#\\"
   c <- oneOf [named, (ScmChar) <$> next]
   commit delimiter `usingError` "Invalid named character"
   return c
 where
   named = oneOf [ string "space"   >> return (ScmChar ' ')
                 , string "newline" >> return (ScmChar '\n') ]

quotedString :: Parser Char ScmValue
quotedString =
   ScmString <$>
      (join bracket (one '"') . many $
         oneOf [one '\\' >> commit (pElem "\\\""), pNotElem "\\\""])

list :: Parser Char Datum
list =
   bracket (one '(') (atmosphere >> one ')') $ do
      datums <- many datum
      if null datums
         then return$ UnevaledApp datums
         else do
            atmosphere
            dot <- optional (one '.')
            if isJust dot
               then DottedList datums <$> commit datum
               else return$ UnevaledApp datums

vector :: Parser Char Datum
vector = do
   one '#'
   UnevaledVec <$> bracket (one '(') (atmosphere >> one ')') (many datum)

-- Pushes back anything relevant for other parsers
delimiter :: Parser Char ()
delimiter = oneOf [whitespaceOrComment, pElem "()\"" >>= reparse.return, eof]

atmosphere :: Parser Char ()
atmosphere = void $ many whitespaceOrComment

whitespaceOrComment :: Parser Char ()
whitespaceOrComment =
   oneOf [ void $ one ' '
         , void newline
         , void comment ]
 where
   newline    = oneOf [ void $ one '\n'
                      , void $ one '\r' >> optional (one '\n') ]
   comment = void $ do
      one ';'
      commit (many notNewline >> (void newline `onFail` return ()))

   notNewline = satisfy (`notElem` "\n\r")

one :: Eq a => a -> Parser a a
one = satisfy . (==)

string :: Eq a => [a] -> Parser a [a]
string []     = return []
string (x:xs) = do
   c <- one x
   return (c:) `apply` string xs

eof :: Parser t ()
eof = do
   x <- optional next
   case x of
        Just c  -> reparse [c] >> fail "Expected EOF"
        Nothing -> return ()

pElem :: Eq a => [a] -> Parser a a
pElem = satisfy . flip elem

pNotElem :: Eq a => [a] -> Parser a a
pNotElem = satisfy . flip notElem

usingError :: Parser a b -> String -> Parser a b
usingError p = adjustErr p . const
