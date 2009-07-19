-- File created: 2009-07-11 20:29:49

module Haschoo.Parser (program, number) where

import Control.Applicative ((<$>))
import Control.Monad       (join)
import Data.Char           ( isDigit, isHexDigit, isOctDigit, isSpace
                           , digitToInt, toLower)
import Data.Complex        (Complex((:+)), mkPolar)
import Data.Maybe          (fromJust, fromMaybe, isJust)
import Data.Ratio          ((%))
import Numeric             (readInt)
import Text.ParserCombinators.Poly.Plain
   ( Parser, next, apply, satisfy, discard, commit, adjustErr, onFail, reparse
   , many, many1, oneOf, oneOf', bracket, indent, optional)

import Haschoo.Datum (Datum(..), ScmValue(..))
import Haschoo.Utils (void)

program :: Parser Char [Datum]
program = datums `discard` eof

datums :: Parser Char [Datum]
datums = atmosphere >> (commit . many $ datum `discard` commit atmosphere)

datum :: Parser Char Datum
datum = do
   quotes <- concat <$> many (oneOf (map string ["'", "`", ",@", ","])
                                 `discard` commit atmosphere)

   dat <- oneOf' [ ("identifier", ident)
                 , ("list", list)
                 , ("vector", vector)
                 , ("value", value) ]

   return $ quote quotes dat
 where
   quote []            = id
   quote ('\''    :qs) = Quoted       . quote qs
   quote ('`'     :qs) = QuasiQuoted  . quote qs
   quote (',' :'@':qs) = FlatUnQuoted . quote qs
   quote (','     :qs) = UnQuoted     . quote qs
   quote _             = error "Parser.quote :: internal error"

value :: Parser Char Datum
value = Evaluated <$> oneOf [bool, number 10, character, quotedString]

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
      initial = ['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*/:<=>?^_~"

bool :: Parser Char ScmValue
bool = one '#' >> ScmBool . (== 't') <$> (pElem "ft")

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
         oneOf [ one '\\' >> commit (pElem "\\\"")
                                `usingError` "Invalid escaped character"
               , satisfy (/= '"') ])

list :: Parser Char Datum
list =
   bracket (one '(') (atmosphere >> one ')') $ do
      dats <- datums `adjustErr` (("In a list:\n"++) . indent 2)
      if null dats
         then return$ UnevaledApp dats
         else do
            atmosphere
            dot <- optional (one '.')
            if isJust dot
               then DottedList dats <$> (commit atmosphere >> datum)
               else return$ UnevaledApp dats

vector :: Parser Char Datum
vector = do
   one '#'
   UnevaledVec <$> bracket (one '(') (atmosphere >> one ')') datums

number :: Int -> Parser Char ScmValue
number defRadix = do
   (radix,exact) <- prefix

   let gotPrefix = isJust radix || isJust exact
   n <- (if gotPrefix then commit else id) $ complex (fromMaybe defRadix radix)

   delimiter

   return$ if fromMaybe True exact
              then n
              else case n of
                        ScmInt x -> ScmReal (fromInteger  x)
                        ScmRat x -> ScmReal (fromRational x)
                        _        -> n
 where
   prefix = do
      r <- radix
      e <- exactness
      -- They can be in either order
      flip (,) e <$> if isJust r
                        then return r
                        else radix
    where
      radix     = f [('b',2), ('o',8), ('d',10), ('x',16)]
      exactness = f [('e',True), ('i',False)]
      f xs = optional $ do
         one '#'
         fromJust . (`lookup` xs) <$> pElem (map fst xs)

   complex radix =
      oneOf [ do n <- sreal radix
                 one 'i'
                 return (mkImaginary n)

            , do a  <- real radix
                 at <- commit $ optional (one '@')
                 if isJust at
                    then mkComplex mkPolar a <$> real radix
                    else do
                       b <- optional $
                               oneOf [ imaginaryUnit
                                     , do n <- sreal radix
                                          commit (one 'i')
                                          return n ]

                       return $ case b of
                                     Nothing -> a
                                     Just n  -> mkComplex (:+) a n

            , mkImaginary <$> imaginaryUnit ]
    where
      imaginaryUnit = do
         neg <- sign
         one 'i'
         return (ScmInt neg)

      mkImaginary     = mkComplex (:+) (ScmInt 0)
      mkComplex f a b = ScmComplex $ f (toDouble a) (toDouble b)

   toDouble (ScmInt  i) = fromInteger  i
   toDouble (ScmRat  r) = fromRational r
   toDouble (ScmReal r) = r
   toDouble _           = error "number.toDouble :: internal error"

   real radix = do
      neg <- optional sign
      applySign (fromMaybe 1 neg) <$> ureal radix

   sreal radix = do
      neg <- sign
      applySign neg <$> ureal radix

   applySign neg (ScmInt  n) = ScmInt  (fromIntegral neg * n)
   applySign neg (ScmRat  n) = ScmRat  (fromIntegral neg * n)
   applySign neg (ScmReal n) = ScmReal (fromIntegral neg * n)
   applySign _   _           = error "number.applySign :: іnternal error"

   ureal radix = oneOf [ string "nan.#" >> return (ScmReal $ 0/0)
                       , string "inf.#" >> return (ScmReal $ 1/0)
                       , decimal radix
                       , do a <- uint radix
                            b <- optional $ one '/' >> uint radix
                            case b of
                                 Nothing ->
                                    if radix == 10
                                       then tryExponent a
                                       else return a
                                 Just n  -> return (mkRatio a n) ]

   mkRatio (ScmInt a) (ScmInt b) = ScmRat (a % b)
   mkRatio _          _          = error "number.mkRatio :: internal error"

   decimal radix | radix /= 10 = fail "Decimal outside radix 10"
                 | otherwise   = do
      n <- oneOf [ do one '.'
                      n <- many1 (digit 10)
                      many (one '#')
                      return . ScmReal $ readPostDecimal n

                 , do a <- many1 (digit 10)
                      one '.'
                      b <- many (digit 10)
                      many (one '#')
                      return . ScmReal $ readDecimal a b

                 , do n <- many1 (digit 10)
                      hashes <- many1 (one '#')
                      one '.'
                      hashes2 <- many (one '#')
                      return . inexactHashes (hashes ++ hashes2) . ScmInt $
                         readInteger 10 (n ++ map (const '0') hashes)
                 ]
      tryExponent n

   tryExponent n = do
      ex <- optional $ do pElem "esfdlESFDL" -- Ignore the exponent: all Double
                          neg <- optional sign
                          xs  <- many1 (digit 10)
                          return$ (fromMaybe 1 neg) * readInteger 10 xs
      return$ case ex of
                   Nothing -> n
                   Just e  -> ScmReal (10^^e * toDouble n)

   uint radix = do
      n <- many1 (digit radix)
      hashes <- many (one '#')
      return . inexactHashes hashes . ScmInt $
         readInteger radix (n ++ map (const '0') hashes)

   -- If any # were present, the value is inexact (R5RS 6.2.4)
   inexactHashes :: String -> ScmValue -> ScmValue
   inexactHashes [] = id
   inexactHashes _  = ScmReal . toDouble

   digit :: Int -> Parser Char Char
   digit 2  = pElem "01"
   digit 8  = satisfy isOctDigit
   digit 10 = satisfy isDigit
   digit 16 = satisfy isHexDigit
   digit _  = error "number.digit :: internal error"

   sign = (\c -> if c == '+' then 1 else -1) <$> pElem "+-"

   -- These read functions all assume correctly formed input

   readInteger :: Int -> String -> Integer
   readInteger radix =
      fst.head . readInt (fromIntegral radix) (const True) digitToInt

   readPostDecimal :: String -> Double
   readPostDecimal [] = 0
   readPostDecimal xs = fromInteger (readInteger 10 xs) / (10 ^ (length xs))

   readDecimal :: String -> String -> Double
   readDecimal as bs = fromInteger (readInteger 10 as) + readPostDecimal bs

-- Pushes back anything relevant for other parsers
delimiter :: Parser Char ()
delimiter = oneOf [whitespaceOrComment, pElem "()\"" >>= reparse.return, eof]
   `usingError` "Invalid delimiter"

atmosphere :: Parser Char ()
atmosphere = void $ many whitespaceOrComment

whitespaceOrComment :: Parser Char ()
whitespaceOrComment =
   oneOf [ void newline
         , void (satisfy isSpace)
         , void comment ]
 where
   newline = oneOf [ void $ one '\n'
                   , void $ one '\r' >> optional (one '\n') ]
   comment = void $ do
      one ';'
      commit (many notNewline >> (void newline `onFail` return ()))

   notNewline = satisfy (`notElem` "\n\r")

one :: Char -> Parser Char Char
one = satisfyNoCase . (==)

string :: String -> Parser Char String
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

pElem :: [Char] -> Parser Char Char
pElem = satisfyNoCase . flip elem

satisfyNoCase :: (Char -> Bool) -> Parser Char Char
satisfyNoCase p = do
   c <- toLower <$> next
   if p c then return c else fail "satisfy failed"

usingError :: Parser a b -> String -> Parser a b
usingError p = adjustErr p . const
