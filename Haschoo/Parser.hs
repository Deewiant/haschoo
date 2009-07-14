-- File created: 2009-07-11 20:29:49

module Haschoo.Parser (parser) where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Text.ParserCombinators.Poly.Plain

import Haschoo.ScmValue (ScmValue(..))

parser :: Parser Char [ScmValue]
parser = do
   ds <- many value
   eof
   return ds

value :: Parser Char ScmValue
value = do
   atmosphere
   quoted <- optional (one '\'' >> commit atmosphere)
   dat    <- oneOf [ident, bool, number, character, quotedString, list, vector]
   return$ if isJust quoted
              then ScmQuoted dat
              else dat

ident :: Parser Char ScmValue
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

list :: Parser Char ScmValue
list =
   bracket (one '(') (atmosphere >> one ')') $ do
      values <- many value
      if null values
         then return$ Application values
         else do
            atmosphere
            dot <- optional (one '.')
            if isJust dot
               then DottedList values <$> commit value
               else return$ Application values

vector :: Parser Char ScmValue
vector = do
   one '#'
   ScmVector <$> bracket (one '(') (atmosphere >> one ')') (many value)

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

void :: Functor f => f a -> f ()
void = fmap (const ())
