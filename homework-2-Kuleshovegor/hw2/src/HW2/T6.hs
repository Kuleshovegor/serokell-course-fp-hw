{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module HW2.T6
  ( ParseError (..)
  , Parser (..)
  , pAbbr
  , pChar
  , pEof
  , parseError
  , parseExpr
  , runP
  ) where
import Control.Applicative (optional)
import Control.Monad (foldM, mfilter)
import qualified Data.Char
import Data.Maybe (fromMaybe)
import Data.Scientific (scientific, toRealFloat)
import GHC.Base (Alternative (empty, some, (<|>)), MonadPlus)
import GHC.Natural (Natural)
import GHC.OldList (foldl')
import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (Val))
import HW2.T5 (ExceptState (..), throwExceptState)
import Data.Foldable (asum)

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P exceptState) string =
  case runES exceptState (0, string) of
    Error errorE         -> Error errorE
    Success (value :# _) -> Success value


pChar :: Parser Char
pChar = P $ ES \(position, string) ->
  case string of
    []                  -> Error (ErrorAtPos position)
    (headStr : tailStr) -> Success (headStr :# (position + 1, tailStr))

parseError :: Parser a
parseError = P (throwExceptState $ ErrorAtPos 0)

instance Alternative Parser where
  empty = parseError

  (<|>) (P exceptState1) (P exceptState2) = P (ES (\state ->
    case runES exceptState1 state of
      Error _       -> runES exceptState2 state
      Success value -> Success value))

instance MonadPlus Parser   -- No methods.

pEof :: Parser ()
pEof = P $ ES \(position, string) ->
  case string of
    []    -> Success $ (() :# (position, string))
    _ : _ -> Error $ ErrorAtPos position

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  return abbr

charDigitToInt :: Char -> Int
charDigitToInt = \case
    '0'    -> 0
    '1'    -> 1
    '2'    -> 2
    '3'    -> 3
    '4'    -> 4
    '5'    -> 5
    '6'    -> 6
    '7'    -> 7
    '8'    -> 8
    '9'    -> 9
    symbol -> error $ "Digit character expected, but found" ++ [symbol]

pDigits :: Parser String
pDigits = some (mfilter Data.Char.isDigit pChar)

strToInteger :: String -> Integer
strToInteger str =
   foldl' (\res elm -> res * 10 + toInteger (charDigitToInt elm)) 0 str

getParserChar :: Char -> Parser Char
getParserChar c = mfilter (== c) pChar

scipWhitespace :: Parser ()
scipWhitespace = do
  _ <- optional (some $ getParserChar ' ')
  return ()

pDot :: Parser Char
pDot = getParserChar '.'

pDouble :: Parser Double
pDouble = do
  beforeDot <- pDigits
  _ <- optional pDot
  afterDotOpt <- optional pDigits
  let afterDot = fromMaybe [] afterDotOpt
  let intRes = strToInteger $ beforeDot ++ afterDot
  let afterDotLen = length afterDot
  return $ toRealFloat $ scientific intRes (-afterDotLen)

pVal :: Parser Expr
pVal = do
  scipWhitespace
  resultValue <- pDouble
  return $ Val resultValue

pBracets :: Parser Expr
pBracets = do
  scipWhitespace
  _ <- getParserChar '('
  scipWhitespace
  resultExpr <- pExpr
  scipWhitespace
  _ <- getParserChar ')'
  return resultExpr

pPrefix :: String -> Parser String
pPrefix prefString = do
    reverseResPref <- foldM (\resultStr curChar -> do
      a <- getParserChar curChar
      return $ a : resultStr) [] prefString
    return $ reverse reverseResPref

parseExpr :: String -> Except ParseError Expr
parseExpr string = runP (pExpr <* pEof) string

charToBinaryOperator :: Char -> (Expr -> Expr -> Expr)
charToBinaryOperator c = case c of
  '+'           -> (+)
  '-'           -> (-)
  '*'           -> (*)
  '/'           -> (/)
  wrongOperator -> error $ "unexpected binary operator: " ++ [wrongOperator]

charToUnaryOperator :: String -> (Expr -> Expr)
charToUnaryOperator c = case c of
  "abs"         -> (abs)
  "signum"      -> (signum)
  wrongOperator -> error $ "unexpected unary operator: " ++ wrongOperator

pExpr :: Parser Expr
pExpr = do
  scipWhitespace
  resultExpr <- pTerm
  scipWhitespace
  pMore ['+', '-'] pTerm resultExpr

pTerm :: Parser Expr
pTerm = do
  scipWhitespace
  resultExpr <- pMultiplier
  pMore ['*', '/'] pMultiplier resultExpr

pMore :: [Char] -> Parser Expr -> Expr -> Parser Expr
pMore operatorsChar nextParser expr = do
  scipWhitespace
  operator <- charToBinaryOperator <$> (asum $ fmap getParserChar operatorsChar)
  scipWhitespace
  rightArg <- nextParser
  scipWhitespace
  pMore operatorsChar nextParser (operator expr rightArg)
  <|> return expr

pUnary :: [String] -> Parser Expr
pUnary operatorsStrings = do
  scipWhitespace
  operator <- charToUnaryOperator <$> (asum $ fmap pPrefix operatorsStrings)
  scipWhitespace
  argument <- pMultiplier
  return $ operator argument

pMultiplier :: Parser Expr
pMultiplier = do
  scipWhitespace
  pUnary ["abs", "signum"] <|> pVal <|> pBracets
