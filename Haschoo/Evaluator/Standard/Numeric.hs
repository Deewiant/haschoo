-- File created: 2009-07-15 21:15:44

{-# LANGUAGE Rank2Types #-}

module Haschoo.Evaluator.Standard.Numeric
   (procedures, isNumeric, isExact, numEq, asInt) where

import Control.Arrow ((&&&), (***))
import Control.Monad (ap, foldM)
import Data.Char     (intToDigit)
import Data.Complex  ( Complex((:+)), mkPolar
                     , imagPart, realPart, phase, magnitude)
import Data.Ratio    (numerator, denominator, approxRational)
import Data.Function (on)
import Numeric       (showIntAtBase, showSigned)
import Text.ParserCombinators.Poly.Plain (runParser)

import qualified Haschoo.Parser as Parser
import           Haschoo.Types           (ScmValue(..))
import           Haschoo.Utils           (ErrOr, allM, ($<), (.:))
import           Haschoo.Evaluator.Utils (tooFewArgs, tooManyArgs)

procedures :: [(String, ScmValue)]
procedures = map (\(a,b) -> (a, ScmFunc a (return . b)))
   [ ("number?",   fmap ScmBool . scmIsNumber)
   , ("complex?",  fmap ScmBool . scmIsNumber)
   , ("real?",     fmap ScmBool . scmIsReal)
   , ("rational?", fmap ScmBool . scmIsRational)
   , ("integer?",  fmap ScmBool . scmIsInteger)

   , "exact?"   $< id &&& fmap  ScmBool        .: scmIsExact
   , "inexact?" $< id &&& fmap (ScmBool . not) .: scmIsExact

   , ("=", fmap ScmBool . scmNumEq)
   , "<"  $< id &&& fmap ScmBool .: scmCompare (<)
   , ">"  $< id &&& fmap ScmBool .: scmCompare (>)
   , "<=" $< id &&& fmap ScmBool .: scmCompare (<=)
   , ">=" $< id &&& fmap ScmBool .: scmCompare (>=)

   , ("zero?",     fmap ScmBool . scmIsZero)
   , ("positive?", fmap ScmBool . scmIsPos)
   , ("negative?", fmap ScmBool . scmIsNeg)
   , "even?" $< id &&& fmap  ScmBool        .: scmIsEven
   , "odd?"  $< id &&& fmap (ScmBool . not) .: scmIsEven

   , ("max", scmMax)
   , ("min", scmMin)

   , ("+", scmPlus)
   , ("-", scmMinus)
   , ("*", scmMul)
   , ("/", scmDiv)

   , ("quotient",  scmQuot)
   , ("remainder", scmRem)
   , ("modulo",    scmMod)

   , ("gcd", scmGcd)
   , ("lcm", scmLcm)

   , ("numerator",   scmNumerator)
   , ("denominator", scmDenominator)

   , ("floor",    scmFloor)
   , ("ceiling",  scmCeil)
   , ("truncate", scmTrunc)
   , ("round",    scmRound)

   , ("rationalize", scmRationalize)

   , ("exp",  scmExp)
   , ("log",  scmLog)
   , ("sin",  scmSin)
   , ("cos",  scmCos)
   , ("tan",  scmTan)
   , ("asin", scmAsin)
   , ("acos", scmAcos)
   , ("atan", scmAtan)

   , ("sqrt", scmSqrt)
   , ("expt", scmExpt)

   , ("make-rectangular", scmMakeRectangular)
   , ("make-polar",       scmMakePolar)
   , ("real-part",        scmRealPart)
   , ("imag-part",        scmImagPart)
   , ("magnitude",        scmNorm)
   , ("angle",            scmAngle)

   , ("exact->inexact", scmToInexact)
   , ("inexact->exact", scmToExact)

   , ("number->string", scmToString)
   , ("string->number", scmToNumber)
   ]

---- Predicates

scmIsNumber, scmIsReal, scmIsRational, scmIsInteger :: [ScmValue] -> ErrOr Bool
scmIsNumber [x] = Right $ isNumeric x
scmIsNumber []  = tooFewArgs  "number?"
scmIsNumber _   = tooManyArgs "number?"

scmIsReal [ScmComplex (_ :+ b)] = Right $ b == 0
scmIsReal [x]                   = Right $ isNumeric x
scmIsReal []                    = tooFewArgs  "real?"
scmIsReal _                     = tooManyArgs "real?"

scmIsRational [ScmRat _] = Right True
scmIsRational [_]        = Right False
scmIsRational []         = tooFewArgs "rational?"
scmIsRational _          = tooManyArgs "rational?"

scmIsInteger [x] = Right $ isInteger x
scmIsInteger []  = tooFewArgs  "integer?"
scmIsInteger _   = tooManyArgs "integer?"

scmIsExact :: String -> [ScmValue] -> ErrOr Bool
scmIsExact _ [x] | isNumeric x = Right $ isExact x
scmIsExact s [_]               = notNum s
scmIsExact s []                = tooFewArgs s
scmIsExact s _                 = tooManyArgs s

isExact :: ScmValue -> Bool
isExact (ScmInt _) = True
isExact (ScmRat _) = True
isExact _          = False

---- Comparison

scmNumEq :: [ScmValue] -> ErrOr Bool
scmNumEq [] = tooFewArgs "="
scmNumEq xs = allM f . (zip`ap`tail) $ xs
 where
   f (a,b) = if isNumeric a && isNumeric b
                then Right $ numEq a b
                else notNum "="

numEq :: ScmValue -> ScmValue -> Bool
numEq x y =
   case pairScmComplex x y of
        Right (ScmInt     a, ScmInt     b) -> a == b
        Right (ScmRat     a, ScmRat     b) -> a == b
        Right (ScmReal    a, ScmReal    b) -> a == b
        Right (ScmComplex a, ScmComplex b) -> a == b
        _ -> error "numEq :: the impossible happened"

scmCompare :: (forall a. Real a => a -> a -> Bool)
           -> String -> [ScmValue] -> ErrOr Bool
scmCompare _ s [] = tooFewArgs s
scmCompare p s xs = allM f . (zip`ap`tail) $ xs
 where
   f (a,b) = case liftScmRealA2 p a b of
                  Left _ -> notReal s
                  x      -> x

scmIsZero :: [ScmValue] -> ErrOr Bool
scmIsZero [ScmInt     0]    = Right True
scmIsZero [ScmRat     0]    = Right True
scmIsZero [ScmReal    0]    = Right True
scmIsZero [ScmComplex 0]    = Right True
scmIsZero [x] | isNumeric x = Right False
scmIsZero [_]               = notNum "positive?"
scmIsZero _                 = tooManyArgs "zero?"

scmIsPos, scmIsNeg :: [ScmValue] -> ErrOr Bool
scmIsPos [x] = scmCompare (>) "positive?" [x, ScmInt 0]
scmIsPos _   = tooManyArgs "positive?"

scmIsNeg [x] = scmCompare (<) "negative?" [x, ScmInt 0]
scmIsNeg _   = tooManyArgs "negative?"

scmIsEven :: String -> [ScmValue] -> ErrOr Bool
scmIsEven s [x] = if isInteger x
                     then (scmIsZero.return) =<< scmMod [x, ScmInt 2]
                     else notInt s
scmIsEven s _   = tooManyArgs s

scmMax, scmMin :: [ScmValue] -> ErrOr ScmValue
scmMax = scmMinMax max "max"
scmMin = scmMinMax min "min"

scmMinMax :: (forall a. Real a => a -> a -> a) -> String
          -> [ScmValue] -> ErrOr ScmValue
scmMinMax _ s []     = tooFewArgs s
scmMinMax f s (x:xs) = if isNumeric x then foldM go x xs else notNum s
 where
   go n a = if isNumeric a
               then case liftScmReal2 f n a of
                         Left _ -> notReal s
                         m      -> m
               else notNum s

---- +-*/

scmPlus, scmMinus, scmMul, scmDiv :: [ScmValue] -> ErrOr ScmValue
scmPlus [] = Right $ ScmInt 0
scmPlus xs = foldM go (ScmInt 0) xs
 where
   go n x = if isNumeric x then liftScmNum2 (+) n x else notNum "+"

scmMinus []                        = tooFewArgs "-"
scmMinus (x:_) | not (isNumeric x) = notNum "-"
scmMinus [x]                       = liftScmNum negate x
scmMinus (x:xs)                    = foldM go x xs
 where
   go n a = if isNumeric a then liftScmNum2 (-) n a else notNum "-"

scmMul [] = Right $ ScmInt 1
scmMul xs = foldM go (ScmInt 1) xs
 where
   go n x = if isNumeric x then liftScmNum2 (*) n x else notNum "*"

scmDiv []     = tooFewArgs "/"
scmDiv (x:xs) =
   unint x >>= case xs of
                    []  -> liftScmFrac recip
                    _:_ -> flip (foldM go) xs
 where
   go n a = unint a >>= liftScmFrac2 (/) n

   unint (ScmInt n) = Right $ ScmRat (fromInteger n)
   unint n          = if isNumeric x then Right n else notNum "/"

---- quot rem mod

scmQuot, scmRem, scmMod :: [ScmValue] -> ErrOr ScmValue
scmQuot = scmQuotRemMod quot "quotient"
scmRem  = scmQuotRemMod rem "remainder"
scmMod  = scmQuotRemMod mod "modulo"

scmQuotRemMod :: (Integer -> Integer -> Integer) -> String
              -> [ScmValue] -> ErrOr ScmValue
scmQuotRemMod f s [x,y] = do
   (e1,a) <- asInt s x
   (e2,b) <- asInt s y
   return . (if e1 && e2 then ScmInt else ScmReal . fromInteger) $ f a b
scmQuotRemMod _ s (_:_:_) = tooManyArgs s
scmQuotRemMod _ s _       = tooFewArgs  s

---- gcd lcm

scmGcd, scmLcm :: [ScmValue] -> ErrOr ScmValue
scmGcd = scmGcdLcm gcd "gcd"
scmLcm = scmGcdLcm lcm "lcm"

scmGcdLcm :: (Integer -> Integer -> Integer) -> String
          -> [ScmValue] -> ErrOr ScmValue
scmGcdLcm f s = fmap fin . foldM go (True,0)
 where
   go  (exact, n) = fmap ((exact &&) *** f n) . asInt s
   fin (exact, n) = if exact then ScmInt n else ScmReal (fromInteger n)

---- numerator denominator

scmNumerator, scmDenominator :: [ScmValue] -> ErrOr ScmValue
scmNumerator   = scmNumerDenom numerator   "numerator"
scmDenominator = scmNumerDenom denominator "denominator"

scmNumerDenom :: (Rational -> Integer) -> String
              -> [ScmValue] -> ErrOr ScmValue
scmNumerDenom f s [n] =
   case n of
        ScmInt      x       -> Right . ScmInt  $ f (fromInteger x)
        ScmRat      x       -> Right . ScmInt  $ f x
        ScmReal     x       -> Right . ScmReal . fromInteger $ f (realToFrac x)
        ScmComplex (x :+ 0) -> Right . ScmReal . fromInteger $ f (realToFrac x)
        _                   -> notRat s

scmNumerDenom _ s [] = tooFewArgs s
scmNumerDenom _ s _  = tooManyArgs s

---- floor ceil trunc round

scmFloor, scmCeil, scmTrunc, scmRound :: [ScmValue] -> ErrOr ScmValue
scmFloor = scmGenericRound floor    "floor"
scmCeil  = scmGenericRound ceiling  "ceiling"
scmTrunc = scmGenericRound truncate "truncate"
scmRound = scmGenericRound round    "round"

scmGenericRound :: (forall a. RealFrac a => a -> Integer) -> String
                -> [ScmValue] -> ErrOr ScmValue
scmGenericRound _ _ [ScmInt     x] = Right . ScmInt                $ x
scmGenericRound f _ [ScmRat     x] = Right . ScmInt                $ f x
scmGenericRound f _ [ScmReal    x] = Right . ScmReal . fromInteger $ f x
scmGenericRound _ s [_]            = notReal s
scmGenericRound _ s []             = tooFewArgs s
scmGenericRound _ s _              = tooManyArgs s

---- rationalize

scmRationalize :: [ScmValue] -> ErrOr ScmValue
scmRationalize (_:_:_:_) = tooManyArgs "rationalize"
scmRationalize [x,y]     =
   case liftScmRealFracB2 approxRational x y of
        Left _           -> notReal "rationalize"
        Right (True , n) -> Right (ScmRat  n)
        Right (False, n) -> Right (ScmReal (fromRational n))

scmRationalize _         = tooFewArgs "rationalize"

---- exp log sin cos tan asin acos atan sqrt

scmExp, scmLog,
 scmSin,  scmCos,  scmTan,
 scmAsin, scmAcos, scmAtan,
 scmSqrt :: [ScmValue] -> ErrOr ScmValue

scmExp  = scmComplex1 exp  "exp"
scmLog  = scmComplex1 log  "log"
scmSin  = scmComplex1 sin  "sin"
scmCos  = scmComplex1 cos  "cos"
scmTan  = scmComplex1 tan  "tan"
scmAsin = scmComplex1 asin "asin"
scmAcos = scmComplex1 acos "acos"

scmAtan [y,x] =
   case pairScmReal x y of
        Left  _                      -> notReal "atan"
        Right (ScmReal a, ScmReal b) -> Right (ScmReal $ phase $ a :+ b)
        Right (ScmInt a, ScmInt b)   ->
           Right (ScmReal $ phase $ fromInteger a :+ fromInteger b)
        Right (ScmRat a, ScmRat b)   ->
           Right (ScmReal $ phase $ fromRational a :+ fromRational b)
        Right _                      -> error "scmAtan :: internal error"

scmAtan xs    = scmComplex1 atan "atan" xs

scmSqrt = scmComplex1 sqrt "sqrt"

scmComplex1 :: (Complex Double -> Complex Double) -> String
            -> [ScmValue] -> ErrOr ScmValue
scmComplex1 f _ [ScmInt     x] = Right . fromComplex $ f (fromInteger  x :+ 0)
scmComplex1 f _ [ScmRat     x] = Right . fromComplex $ f (fromRational x :+ 0)
scmComplex1 f _ [ScmReal    x] = Right . fromComplex $ f (x :+ 0)
scmComplex1 f _ [ScmComplex x] = Right . fromComplex $ f x
scmComplex1 _ s [_]            = notNum s
scmComplex1 _ s []             = tooFewArgs s
scmComplex1 _ s _              = tooManyArgs s

---- expt

scmExpt :: [ScmValue] -> ErrOr ScmValue
scmExpt [x,y] =
   case pairScmComplex x y of
        Left _                              -> notNum "expt"
        Right (ScmInt     a, ScmInt      b) -> Right . ScmInt     $ a ^ b
        Right (ScmReal    a, ScmReal     b) -> Right . ScmReal    $ a ** b
        Right (ScmComplex a, ScmComplex  b) -> Right . ScmComplex $ a ** b
        Right (ScmRat     a, ScmRat      b) ->
           Right . ScmReal $ fromRational a ** fromRational b
        Right _                             -> error "expt :: internal error"

scmExpt (_:_:_) = tooManyArgs "expt"
scmExpt _       = tooFewArgs  "expt"

---- make-rectangular make-polar real-part imag-part magnitude angle

scmMakeRectangular, scmMakePolar :: [ScmValue] -> ErrOr ScmValue
scmMakeRectangular = scmMakeComplex (:+)    "make-rectangular"
scmMakePolar       = scmMakeComplex mkPolar "make-polar"

scmMakeComplex :: (Double -> Double -> Complex Double) -> String
               -> [ScmValue] -> ErrOr ScmValue
scmMakeComplex f s [x,y] =
   case pairScmReal x y of
        Left _                       -> notReal s
        Right (ScmReal a, ScmReal b) -> Right . ScmComplex $ f a b
        Right (ScmRat  a, ScmRat  b) ->
           Right . ScmComplex $ (f `on` fromRational) a b
        Right (ScmInt  a, ScmInt  b) ->
           Right . ScmComplex $ (f `on` fromInteger) a b
        Right _                      ->
           error $ s ++ " :: internal error"

scmMakeComplex _ s (_:_:_) = tooManyArgs s
scmMakeComplex _ s _       = tooFewArgs  s

scmRealPart, scmImagPart, scmNorm, scmAngle :: [ScmValue] -> ErrOr ScmValue
scmRealPart = scmComplexPart realPart  "real-part"
scmImagPart = scmComplexPart imagPart  "imag-part"
scmNorm     = scmComplexPart magnitude "magnitude"
scmAngle    = scmComplexPart phase     "angle"

scmComplexPart :: (Complex Double -> Double) -> String
               -> [ScmValue] -> ErrOr ScmValue
scmComplexPart f _ [ScmComplex x]    = Right $ ScmReal (f x)
scmComplexPart _ _ [x] | isNumeric x = Right x
scmComplexPart _ s [_]               = notNum s
scmComplexPart _ s []                = tooFewArgs s
scmComplexPart _ s _                 = tooManyArgs s

---- exact->inexact inexact->exact

scmToInexact, scmToExact :: [ScmValue] -> ErrOr ScmValue
scmToInexact [ScmInt x]         = Right $ ScmReal (fromInteger  x)
scmToInexact [ScmRat x]         = Right $ ScmReal (fromRational x)
scmToInexact [x] | isNumeric x  = Right x
scmToInexact [_]                = notNum      "exact->inexact"
scmToInexact []                 = tooFewArgs  "exact->inexact"
scmToInexact _                  = tooManyArgs "exact->inexact"

scmToExact [ScmReal    x]     =
   let r = toRational x
    in Right $ if denominator r == 1
                  then ScmInt (numerator r)
                  else ScmRat r
scmToExact [ScmComplex x]     =
   case imagPart x of
        0 -> Right $ ScmReal (realPart x)
        _ -> fail $ "inexact->exact :: implementation restriction: " ++
                    "can't exactify complex numbers"
scmToExact [x] | isNumeric x  = Right x
scmToExact [_]                = notNum      "exact->inexact"
scmToExact []                 = tooFewArgs  "exact->inexact"
scmToExact _                  = tooManyArgs "exact->inexact"

---- number->string string->number

scmToString, scmToNumber :: [ScmValue] -> ErrOr ScmValue

scmToString (x:xs) | isNumeric x =
   case xs of
        []             -> fmap ScmString $ f x 10
        [ScmInt radix] -> if radix `elem` [2,8,10,16]
                             then fmap ScmString $ f x (fromInteger radix)
                             else fail "number->string :: invalid radix"
        [_]            -> notInt      "number->string"
        _              -> tooManyArgs "number->string"
 where
   f (ScmInt i) radix = Right $ showInt radix i
   f (ScmRat r) radix = Right $
      concat [showInt radix (numerator r), "/", showInt radix (denominator r)]

   f (ScmReal r) radix =
      if radix == 10
         then
            -- R5RS 6.2.6 specifies that "the result contains a decimal point"
            -- if inexact and a decimal-point containing result can make it
            -- work
            Right (show r)
         else fail "number->string :: nondecimal radix for inexact real"

   f (ScmComplex r) radix =
      -- As in the ScmReal case
      if radix == 10
         then Right (show r)
         else fail "number->string :: nondecimal radix for inexact complex"

   f _ _ = error "number->string :: the impossible happened"

   showInt radix i = showSigned (showIntAtBase radix intToDigit) 0 i ""

scmToString [] = tooFewArgs "number->string"
scmToString _  = notNum     "number->string"

scmToNumber (ScmString s : xs) =
   case xs of
        []             -> Right $ f s 10
        [ScmInt radix] -> if radix `elem` [2,8,10,16]
                             then Right $ f s (fromInteger radix)
                             else fail "string->number :: invalid radix"
        [_]            -> notInt      "string->number"
        _              -> tooManyArgs "string->number"
 where
   f str radix =
      let (mn,_) = runParser (Parser.number radix) str
       in case mn of
               Right n | isNumeric n -> n
               _                     -> ScmBool False

scmToNumber [] = tooFewArgs "string->number"
scmToNumber _  =
   fail "Nonstring argument to primitive procedure string->number"

-------------

notInt, notNum, notReal, notRat :: String -> ErrOr a
notInt  = fail . ("Noninteger argument to primitive procedure " ++)
notNum  = fail . ("Nonnumeric argument to primitive procedure " ++)
notReal = fail . ("Nonreal argument to primitive procedure " ++)
notRat  = fail . ("Nonrational argument to primitive procedure " ++)

isNumeric, isInteger :: ScmValue -> Bool
isNumeric (ScmInt     _) = True
isNumeric (ScmRat     _) = True
isNumeric (ScmReal    _) = True
isNumeric (ScmComplex _) = True
isNumeric _              = False

isInteger (ScmInt     _)      = True
isInteger (ScmRat     x)      = denominator x == 1
isInteger (ScmReal    x)      = x == fromInteger (round x)
isInteger (ScmComplex (a:+b)) = b == 0 && a == fromInteger (round a)
isInteger _                   = False

asInt :: String -> ScmValue -> ErrOr (Bool, Integer)
asInt s x | not (isInteger x) = notInt s
asInt _ (ScmInt     x)        = Right (True,  x)
asInt _ (ScmRat     x)        = Right (True,  numerator x)
asInt _ (ScmReal    x)        = Right (False, round x)
asInt _ (ScmComplex (x:+0))   = Right (False, round x)
asInt _ _                     = error "This can't happen"

fromComplex :: Complex Double -> ScmValue
fromComplex (x :+ 0) = ScmReal x
fromComplex x        = ScmComplex x

-- Lift ScmValue's numeric types to a common type
--
-- Versions without 'A' at the end also return results in the correct
-- constructor of the two given. Versions with 'B' just return whether the
-- result should be exact or not.
--
-- Ugly and verbose... too lazy to metaize these {{{

--- Num
liftScmNum :: (forall a. Num a => a -> a) -> ScmValue -> ErrOr ScmValue
liftScmNum f (ScmInt     a) = Right . ScmInt     $ f a
liftScmNum f (ScmRat     a) = Right . ScmRat     $ f a
liftScmNum f (ScmReal    a) = Right . ScmReal    $ f a
liftScmNum f (ScmComplex a) = Right . ScmComplex $ f a
liftScmNum _ _ = fail "liftScmNum :: internal error"

liftScmNum2 :: (forall a. Num a => a -> a -> a)
            -> (ScmValue -> ScmValue -> ErrOr ScmValue)
liftScmNum2 f (ScmInt     a) (ScmInt     b) = Right . ScmInt    $ f a b
liftScmNum2 f (ScmRat     a) (ScmRat     b) = Right . ScmRat    $ f a b
liftScmNum2 f (ScmReal    a) (ScmReal    b) = Right . ScmReal   $ f a b
liftScmNum2 f (ScmComplex a) (ScmComplex b) = Right . ScmComplex$ f a b

-- Int+{Rat,Real,Complex}
liftScmNum2 f (ScmInt     a) (ScmRat     b) = Right . ScmRat    $ f (fInt a) b
liftScmNum2 f (ScmRat     a) (ScmInt     b) = Right . ScmRat    $ f a (fInt b)
liftScmNum2 f (ScmInt     a) (ScmReal    b) = Right . ScmReal   $ f (fInt a) b
liftScmNum2 f (ScmReal    a) (ScmInt     b) = Right . ScmReal   $ f a (fInt b)
liftScmNum2 f (ScmInt     a) (ScmComplex b) = Right . ScmComplex$ f (fInt a) b
liftScmNum2 f (ScmComplex a) (ScmInt     b) = Right . ScmComplex$ f a (fInt b)

-- Rat+{Real,Complex}
liftScmNum2 f (ScmRat     a) (ScmReal    b) = Right . ScmReal   $ f (fRat a) b
liftScmNum2 f (ScmReal    a) (ScmRat     b) = Right . ScmReal   $ f a (fRat b)
liftScmNum2 f (ScmRat     a) (ScmComplex b) = Right . ScmComplex$ f (fRat a) b
liftScmNum2 f (ScmComplex a) (ScmRat     b) = Right . ScmComplex$ f a (fRat b)

-- Real+Complex
liftScmNum2 f (ScmReal    a) (ScmComplex b) = Right . ScmComplex$ f (a :+ 0) b
liftScmNum2 f (ScmComplex a) (ScmReal    b) = Right . ScmComplex$ f a (b :+ 0)

liftScmNum2 _ _ _ = fail "liftScmNum2 :: internal error"

--- Frac
liftScmFrac :: (forall a. Fractional a => a -> a) -> ScmValue -> ErrOr ScmValue
liftScmFrac f (ScmRat     a) = Right . ScmRat     $ f a
liftScmFrac f (ScmReal    a) = Right . ScmReal    $ f a
liftScmFrac f (ScmComplex a) = Right . ScmComplex $ f a
liftScmFrac _ _ = fail "liftScmFrac :: internal error"

liftScmFrac2 :: (forall a. Fractional a => a -> a -> a)
             -> (ScmValue -> ScmValue -> ErrOr ScmValue)
liftScmFrac2 f (ScmRat     a) (ScmRat     b) = Right . ScmRat    $ f a b
liftScmFrac2 f (ScmReal    a) (ScmReal    b) = Right . ScmReal   $ f a b
liftScmFrac2 f (ScmComplex a) (ScmComplex b) = Right . ScmComplex$ f a b

-- Rat+{Real,Complex}
liftScmFrac2 f (ScmRat     a) (ScmReal    b) = Right . ScmReal   $ f (fRat a) b
liftScmFrac2 f (ScmReal    a) (ScmRat     b) = Right . ScmReal   $ f a (fRat b)
liftScmFrac2 f (ScmRat     a) (ScmComplex b) = Right . ScmComplex$ f (fRat a) b
liftScmFrac2 f (ScmComplex a) (ScmRat     b) = Right . ScmComplex$ f a (fRat b)

-- Real+Complex
liftScmFrac2 f (ScmReal    a) (ScmComplex b) = Right . ScmComplex$ f (a :+ 0) b
liftScmFrac2 f (ScmComplex a) (ScmReal    b) = Right . ScmComplex$ f a (b :+ 0)

liftScmFrac2 _ _ _ = fail "liftScmFrac2 :: internal error"

--- Real
liftScmReal2 :: (forall a. Real a => a -> a -> a)
             -> (ScmValue -> ScmValue -> ErrOr ScmValue)
liftScmReal2 f x y =
   case pairScmReal x y of
        Right (ScmReal a, ScmReal b) -> Right . ScmReal $ f a b
        Right (ScmRat  a, ScmRat  b) -> Right . ScmRat  $ f a b
        Right (ScmInt  a, ScmInt  b) -> Right . ScmInt  $ f a b
        Left s                       -> Left s
        Right _                      -> fail "liftScmReal2 :: internal error"

liftScmRealA2 :: (forall a. Real a => a -> a -> b)
              -> (ScmValue -> ScmValue -> ErrOr b)
liftScmRealA2 f x y =
   case pairScmReal x y of
        Right (ScmReal a, ScmReal b) -> Right $ f a b
        Right (ScmRat  a, ScmRat  b) -> Right $ f a b
        Right (ScmInt  a, ScmInt  b) -> Right $ f a b
        Left s                       -> Left s
        Right _                      -> fail "liftScmRealA2 :: internal error"

--- RealFrac
liftScmRealFracB2 :: (forall a. RealFrac a => a -> a -> b)
                  -> (ScmValue -> ScmValue -> ErrOr (Bool, b))
liftScmRealFracB2 f x y =
   case pairScmReal x y of
        Right (ScmReal a, ScmReal b) -> Right (False, f a b)
        Right (ScmRat  a, ScmRat  b) -> Right (True,  f a b)
        Right (ScmInt  a, ScmInt  b) ->
           Right (True,  f (fInt a) (fInt b :: Rational))
        Left s                       -> Left s
        Right _                      ->
           fail "liftScmRealFracB2 :: internal error"

pairScmReal :: ScmValue -> ScmValue -> ErrOr (ScmValue, ScmValue)
pairScmReal (ScmComplex a) _ | imagPart a /= 0 = fail "pairScmReal :: complex"
pairScmReal _ (ScmComplex b) | imagPart b /= 0 = fail "pairScmReal :: complex"

pairScmReal a@(ScmInt     _) b@(ScmInt     _) = Right (a,b)
pairScmReal a@(ScmRat     _) b@(ScmRat     _) = Right (a,b)
pairScmReal a@(ScmReal    _) b@(ScmReal    _) = Right (a,b)
pairScmReal   (ScmComplex a)   (ScmComplex b) =
   Right $ ((,) `on` ScmReal . realPart) a b

-- Int+{Rat,Real,Complex}
pairScmReal (ScmInt     a) (ScmRat     b) = Right $ ((,)`on`ScmRat)  (fInt a) b
pairScmReal (ScmRat     a) (ScmInt     b) = Right $ ((,)`on`ScmRat)  a (fInt b)
pairScmReal (ScmInt     a) (ScmReal    b) = Right $ ((,)`on`ScmReal) (fInt a) b
pairScmReal (ScmReal    a) (ScmInt     b) = Right $ ((,)`on`ScmReal) a (fInt b)
pairScmReal (ScmInt     a) (ScmComplex b) =
   Right $ ((,)`on`ScmReal) (fInt a) (realPart b)
pairScmReal (ScmComplex a) (ScmInt     b) =
   Right $ ((,)`on`ScmReal) (realPart a) (fInt b)

-- Rat+{Real,Complex}
pairScmReal (ScmRat     a) (ScmReal    b) = Right $ ((,)`on`ScmReal) (fRat a) b
pairScmReal (ScmReal    a) (ScmRat     b) = Right $ ((,)`on`ScmReal) a (fRat b)
pairScmReal (ScmRat     a) (ScmComplex b) =
   Right $ ((,)`on`ScmReal) (fRat a) (realPart b)
pairScmReal (ScmComplex a) (ScmRat     b) =
   Right $ ((,)`on`ScmReal) (realPart a) (fRat b)

-- Real+Complex
pairScmReal (ScmReal    a) (ScmComplex b) =
   Right $ ((,)`on`ScmReal) a (realPart b)
pairScmReal (ScmComplex a) (ScmReal    b) =
   Right $ ((,)`on`ScmReal) (realPart a) b

pairScmReal _ _ = fail "pairScmReal :: internal error"

pairScmComplex :: ScmValue -> ScmValue -> ErrOr (ScmValue, ScmValue)
pairScmComplex a@(ScmInt     _) b@(ScmInt     _) = Right (a,b)
pairScmComplex a@(ScmRat     _) b@(ScmRat     _) = Right (a,b)
pairScmComplex a@(ScmReal    _) b@(ScmReal    _) = Right (a,b)
pairScmComplex a@(ScmComplex _) b@(ScmComplex _) = Right (a,b)

-- Int+{Rat,Real,Complex}
pairScmComplex (ScmInt     a) (ScmRat     b) = Right$((,)`on`ScmRat) (fInt a) b
pairScmComplex (ScmRat     a) (ScmInt     b) = Right$((,)`on`ScmRat) a (fInt b)
pairScmComplex (ScmInt     a) (ScmReal    b) =
   Right $ ((,)`on`ScmReal) (fInt a) b
pairScmComplex (ScmReal    a) (ScmInt     b) =
   Right $ ((,)`on`ScmReal) a (fInt b)
pairScmComplex (ScmInt     a) (ScmComplex b) =
   Right $ ((,)`on`ScmComplex) (fInt a) b
pairScmComplex (ScmComplex a) (ScmInt     b) =
   Right $ ((,)`on`ScmComplex) a (fInt b)

-- Rat+{Real,Complex}
pairScmComplex (ScmRat     a) (ScmReal    b) =
   Right $ ((,)`on`ScmReal) (fRat a) b
pairScmComplex (ScmReal    a) (ScmRat     b) =
   Right $ ((,)`on`ScmReal) a (fRat b)
pairScmComplex (ScmRat     a) (ScmComplex b) =
   Right $ ((,)`on`ScmComplex) (fRat a) b
pairScmComplex (ScmComplex a) (ScmRat     b) =
   Right $ ((,)`on`ScmComplex) a (fRat b)

-- Real+Complex
pairScmComplex (ScmReal    a) (ScmComplex b) =
   Right $ ((,)`on`ScmComplex) (a:+0) b
pairScmComplex (ScmComplex a) (ScmReal    b) =
   Right $ ((,)`on`ScmComplex) a (b:+0)

pairScmComplex _ _ = fail "pairScmComplex :: internal error"

fInt :: Num a => Integer -> a
fInt = fromInteger

fRat :: Fractional a => Rational -> a
fRat = fromRational

-- }}}
