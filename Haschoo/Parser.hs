-- File created: 2009-07-11 20:29:49

module Haschoo.Parser (parser) where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Text.ParserCombinators.Poly.Plain

import Haschoo.ScmType (ScmType(..))

parser :: Parser Char [ScmType]
parser = do
   ds <- many value
   eof
   return ds

value :: Parser Char ScmType
value = do
   atmosphere
   quoted <- optional (one '\'' >> commit atmosphere)
   dat    <- oneOf [ident, bool, number, character, quotedString, list, vector]
   return$ if isJust quoted
              then ScmQuoted dat
              else dat

ident :: Parser Char ScmType
ident = do
   x <- oneOf [peculiar,ordinary]
   delimiter
   return x
 where
   peculiar = Unevaluated <$> oneOf [return <$> pElem "+-", string "..."]
   ordinary = do
      x  <- pElem initial
      xs <- many (pElem (initial ++ "+-.@" ++ ['0'..'9']))
      return$ Unevaluated (x:xs)
    where
      initial = ['a'..'z'] ++ "!$%&*/:<=>?^_~"

bool :: Parser Char ScmType
bool = one '#' >> ScmBool . (=='t') <$> (pElem "tf")

-- FIXME: only does base 10 integers
number :: Parser Char ScmType
number = do
   x  <- satisfy isDigit
   xs <- commit $ manyFinally (satisfy isDigit) delimiter
   return . ScmInt $ read (x:xs)

character :: Parser Char ScmType
character = do
   string "#\\"
   c <- oneOf [named, (ScmChar) <$> next]
   commit delimiter `usingError` "Invalid named character"
   return c
 where
   named = oneOf [ string "space"   >> return (ScmChar ' ')
                 , string "newline" >> return (ScmChar '\n') ]

quotedString :: Parser Char ScmType
quotedString =
   ScmString <$>
      (join bracket (one '"') . many $
         oneOf [one '\\' >> commit (pElem "\\\""), pNotElem "\\\""])

list :: Parser Char ScmType
list =
   bracket (one '(') (atmosphere >> one ')') $ do
      values <- many value
      if null values
         then return$ ScmList values
         else do
            atmosphere
            dot <- optional (one '.')
            if isJust dot
               then ScmDottedList values <$> commit value
               else return$ ScmList values

vector :: Parser Char ScmType
vector = do
   one '#'
   ScmVector <$> bracket (one '(') (atmosphere >> one ')') (many value)

-- Pushes back anything relevant for other parsers
delimiter :: Parser Char ()
delimiter = oneOf [whitespaceOrComment, pElem "()\"" >>= reparse.return, eof]
 where

atmosphere :: Parser Char ()
atmosphere = void $ many whitespaceOrComment

whitespaceOrComment :: Parser Char ()
whitespaceOrComment =
   oneOf [ void $ one ' '
         , void newline
         , void $ one ';' >> commit (manyFinally notNewline newline) ]
 where
   newline    = oneOf [ void $ one '\n'
                      , void $ one '\r' >> optional (one '\n') ]
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
