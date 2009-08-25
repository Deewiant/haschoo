-- File created: 2009-07-11 20:29:49

module Haschoo.Parser (runParser, programValue, value, number) where

import Control.Applicative                  ((<$>))
import Control.Arrow                        (first)
import Control.Monad                        (liftM2)
import Data.Char                            (digitToInt, toLower)
import Data.Complex                         (Complex((:+)), mkPolar)
import Data.Maybe                           (fromJust, fromMaybe, isJust)
import Data.Ratio                           ((%))
import Numeric                              (readInt)
import Text.ParserCombinators.Parsec hiding (runParser)

import Haschoo.Types (ScmValue(..), toScmString, toScmVector)
import Haschoo.Utils (void, (.:))

runParser :: Parser a -> SourceName -> String -> Either String a
runParser = (either (Left . show) Right .:) . parse

programValue :: Maybe SourcePos -> Parser (Maybe (ScmValue, String, SourcePos))
programValue pos = do
   maybe (return ()) setPosition pos
   optional atmosphere
   v <- (Just <$> value) <|> (eof >> return Nothing)
   case v of
        Nothing -> return Nothing
        Just v' -> liftM2 (Just .: (,,) v') getInput getPosition

values :: Parser [ScmValue]
values = optional atmosphere >> (many $ value `discard` atmosphere)

value :: Parser ScmValue
value = do
   quotes <-
      concat <$> many
         (choice [ string "'"
                 , string "`"
                 , liftM2 (:) (char ',') (string "@" <|> return "") ]
             `discard` atmosphere)

   val <- choice [ ident
                 , list
                 , vector
                 , bool
                 , number 10
                 , character
                 , quotedString ]

   return $ quote quotes val
 where
   quote []            = id
   quote ('\''    :qs) = quoteWith "quote"            qs
   quote ('`'     :qs) = quoteWith "quasiquote"       qs
   quote (',' :'@':qs) = quoteWith "unquote-splicing" qs
   quote (','     :qs) = quoteWith "unquote"          qs
   quote _             = error "Parser.quote :: the impossible happened"

   quoteWith s qs = ScmList . (ScmIdentifier s :) . (:[]) . quote qs

ident :: Parser ScmValue
ident = do
   -- peculiar needs the try due to negative numbers
   x <- try $ choice [ peculiar `discard` try delimiter
                     , ordinary `discard` delimiter ]
   return (ScmIdentifier x)
 where
   peculiar = choice [return <$> oneOf "+-", string "..."]
   ordinary = do
      x  <- oneOf initial
      xs <- many (oneOf (initial ++ "+-.@" ++ ['0'..'9']))
      return (x:xs)
    where
      initial = ['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*/:<=>?^_~"

bool :: Parser ScmValue
bool = ScmBool . (`elem` "tT") <$> try (char '#' >> oneOf "ftFT")

character :: Parser ScmValue
character = do
   try (string "#\\")
   c <- try named <|> anyChar
   delimiter
   return (ScmChar c)
 where
   named = choice [ string "space"   >> return ' '
                  , string "newline" >> return '\n' ]

quotedString :: Parser ScmValue
quotedString =
   fmap toScmString
      . between (try $ char '"') (char '"')
      . many $
          (char '\\' >> (oneOf "\\\"" <?> concat [show "\\"," or ",show "\""]))
      <|> satisfy (/= '"')

list :: Parser ScmValue
list =
   between (try $ char '(') (optional atmosphere >> char ')') $ do
      vals <- values
      if null vals
         then return$ ScmList vals
         else do
            optional atmosphere
            dot <- optionMaybe (char '.')
            if isJust dot
               then do
                  atmosphere
                  end <- value
                  return$ case end of
                               ScmList l -> ScmList (vals ++ l)
                               _         -> ScmDottedList vals end
               else return$ ScmList vals

vector :: Parser ScmValue
vector = do
   try (char '#' >> char '(')
   toScmVector <$> values `discard` (optional atmosphere >> char ')')

number :: Int -> Parser ScmValue
number defRadix = do
   (radix,exact) <- try prefix

   let gotPrefix = isJust radix || isJust exact
   n <- (if gotPrefix then id else try) $ complex (fromMaybe defRadix radix)

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
      radix     = f [('B',2), ('O',8), ('D',10), ('X',16)]
      exactness = f [('E',True), ('I',False)]
      f xs = optionMaybe . try $ do
         char '#'
         let xs' = map (first toLower) xs ++ xs
         fromJust . (`lookup` xs') <$> oneOf (map fst xs')

   complex radix =
      choice [ try $ do n <- sreal radix
                        ncChar 'i'
                        return (mkImaginary n)

             , try $ do a  <- real radix
                        at <- optionMaybe (char '@')
                        if isJust at
                           then mkComplex mkPolar a <$> real radix
                           else do
                              b <- optionMaybe $ try imaginaryUnit <|>
                                      do n <- sreal radix
                                         ncChar 'i'
                                         return n

                              return $ case b of
                                            Nothing -> a
                                            Just n  -> mkComplex (:+) a n

             , mkImaginary <$> imaginaryUnit ]
    where
      imaginaryUnit = do
         neg <- sign
         ncChar 'i'
         return (ScmInt neg)

      mkImaginary     = mkComplex (:+) (ScmInt 0)
      mkComplex f a b = ScmComplex $ f (toDouble a) (toDouble b)

   toDouble (ScmInt  i) = fromInteger  i
   toDouble (ScmRat  r) = fromRational r
   toDouble (ScmReal r) = r
   toDouble _           = error "number.toDouble :: internal error"

   real radix = do
      neg <- optionMaybe sign
      applySign (fromMaybe 1 neg) <$> ureal radix

   sreal radix = do
      neg <- sign
      applySign neg <$> ureal radix

   applySign neg (ScmInt  n) = ScmInt  (fromIntegral neg * n)
   applySign neg (ScmRat  n) = ScmRat  (fromIntegral neg * n)
   applySign neg (ScmReal n) = ScmReal (fromIntegral neg * n)
   applySign _   _           = error "number.applySign :: іnternal error"

   ureal radix = choice [ string "nan.#" >> return (ScmReal $ 0/0)
                        , string "inf.#" >> return (ScmReal $ 1/0)
                        , decimal radix
                        , do a <- uint radix
                             b <- optionMaybe $ char '/' >> uint radix
                             case b of
                                  Nothing ->
                                     if radix == 10
                                        then tryExponent a
                                        else return a
                                  Just n  -> return (mkRatio a n) ]

   mkRatio (ScmInt a) (ScmInt b) = ScmRat (a % b)
   mkRatio _          _          = error "number.mkRatio :: internal error"

   decimal radix | radix /= 10 = fail "Decimal outside radix 10"
                 | otherwise   =
      tryExponent =<<
         (try . choice) [ do char '.'
                             n <- many1 (digitN 10)
                             skipMany (char '#')
                             return . ScmReal $ readPostDecimal n

                        , do a <- many1 (digitN 10)
                             char '.'
                             b <- many (digitN 10)
                             skipMany (char '#')
                             return . ScmReal $ readDecimal a b

                        , do n <- many1 (digitN 10)
                             hashes  <- map (const '0') <$> many1 (char '#')
                             char '.'
                             hashes2 <- many (char '#')
                             return . inexactHashes (hashes ++ hashes2) .
                                ScmInt $ readInteger 10 (n ++ hashes)
                        ]

   tryExponent n = do
      ex <- optionMaybe $ do
               oneOf "esfdlESFDL" -- Ignore the exponent: all Double
               neg <- optionMaybe sign
               xs  <- many1 (digitN 10)
               return$ fromMaybe 1 neg * readInteger 10 xs

      return$ case ex of
                   Nothing -> n
                   Just e  -> ScmReal (10^^e * toDouble n)

   uint radix = do
      n <- many1 (digitN radix)
      hashes <- map (const '0') <$> many (char '#')
      return . inexactHashes hashes . ScmInt $ readInteger radix (n ++ hashes)

   -- If any # were present, the value is inexact (R5RS 6.2.4)
   inexactHashes :: String -> ScmValue -> ScmValue
   inexactHashes [] = id
   inexactHashes _  = ScmReal . toDouble

   digitN :: Int -> Parser Char
   digitN 2  = oneOf "01"
   digitN 8  = octDigit
   digitN 10 = digit
   digitN 16 = hexDigit
   digitN _  = error "number.digitN :: internal error"

   sign = (\c -> if c == '+' then 1 else -1) <$> oneOf "+-"

   -- These read functions all assume correctly formed input

   readInteger :: Int -> String -> Integer
   readInteger radix =
      fst.head . readInt (fromIntegral radix) (const True) digitToInt

   readPostDecimal :: String -> Double
   readPostDecimal [] = 0
   readPostDecimal xs = fromInteger (readInteger 10 xs) / (10 ^ length xs)

   readDecimal :: String -> String -> Double
   readDecimal as bs = fromInteger (readInteger 10 as) + readPostDecimal bs

-- Pushes back anything relevant for other parsers
delimiter :: Parser ()
delimiter = choice [ whitespaceOrComment
                   , void . lookAhead . try $ oneOf "()\""
                   , try eof ]

atmosphere :: Parser ()
atmosphere = skipMany whitespaceOrComment

whitespaceOrComment :: Parser ()
whitespaceOrComment =
   choice [ void newline
          , char '\r' >> optional newline
          , void space
          , char ';' >> skipMany (noneOf "\n\r") ]

ncChar :: Char -> Parser Char
ncChar c = satisfy $ (== c) . toLower

discard :: Parser a -> Parser b -> Parser a
discard a b = a >>= \a' -> b >> return a'
