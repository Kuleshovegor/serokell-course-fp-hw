{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser
  ( pString
  , parse
  ) where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import Data.Text (Text, pack)
import Data.Void (Void)
import HW3.Base (HiExpr (HiExprApply, HiExprValue), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec, choice,
                        manyTill, optional, runParser, sepBy, (<|>))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

pHiFun :: Parser HiFun
pHiFun = choice [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"
  , try $ HiFunNot <$ do
      notFollowedBy $ string "not-greater-than"
      notFollowedBy $ string "not-less-than"
      notFollowedBy $ string "not-equals"
      string "not"
  , HiFunAnd <$ string "and"
  , HiFunOr <$ string "or"
  , HiFunLessThan <$ string "less-than"
  , HiFunGreaterThan <$ string "greater-than"
  , HiFunEquals <$ string "equals"
  , try $ HiFunNotLessThan <$ string "not-less-than"
  , try $ HiFunNotGreaterThan <$ string "not-greater-than"
  , try $ HiFunNotEquals <$ string "not-equals"
  , HiFunIf <$ string "if"
  , HiFunLength <$ string "length"
  , HiFunToUpper <$ string "to-upper"
  , HiFunToLower <$ string "to-lower"
  , HiFunReverse <$ string "reverse"
  , HiFunTrim <$ string "trim"
  , HiFunList <$ string "list"
  , HiFunRange <$ string "range"
  , HiFunFold <$ string "fold"
  ]

pRational :: Parser Rational
pRational = do
  num <- L.signed sc L.scientific
  return $ toRational num

pBool :: Parser Bool
pBool = choice [ True <$ string "true"
  , False <$ string "false"]

pString :: Parser Text
pString = do
  char '"'
  res <- manyTill L.charLiteral (char '"')
  return $ pack res

pList :: Parser HiExpr
pList = do
    sc
    string "["
    sc
    elems <- sepBy pHiExpr (do
      sc
      string ","
      sc)
    sc
    string "]"
    sc
    return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) elems

pHiValue :: Parser HiValue
pHiValue = choice [try (HiValueNumber <$> pRational)
  , try (HiValueFunction <$> pHiFun)
  , try (HiValueBool <$> pBool)
  , try (HiValueString <$> pString)
  , HiValueNull <$ string "null"]

pMoreHiFunExpr :: HiExpr -> Parser HiExpr
pMoreHiFunExpr exps = do
  sc
  args <- optional (do
    sc
    string "("
    sc
    args <- sepBy pHiExpr (do
      sc
      string ","
      sc)
    sc
    string ")"
    sc
    return $ args)
  sc
  case args of
    Nothing -> return exps
    Just ar -> pMoreHiFunExpr (HiExprApply exps ar)

pHiFunExpr :: Parser HiExpr
pHiFunExpr = do
  sc
  val <-do
    sc
    (try pList)
    <|> (try $ HiExprValue <$> do
      notFollowedBy $ string "("
      pHiValue)
    <|> do
      sc
      string "("
      sc
      res <- pHiValue
      sc
      string ")"
      sc
      return $ HiExprValue res
  sc
  pMoreHiFunExpr val

pHiExpr :: Parser HiExpr
pHiExpr = makeExprParser term table

term :: Parser HiExpr
term = try pHiFunExpr
  <|> (try $ do
    sc
    string "("
    sc
    res <- pHiExpr
    sc
    string ")"
    sc
    return res)

table :: [[Operator Parser HiExpr]]
table = [ [ binaryL  (string "*")  HiFunMul
          , binaryL (do
            notFollowedBy (string "/=")
            (string "/")) HiFunDiv
          ]
        , [ binaryL (string "+")  HiFunAdd
          , binaryL (string "-")  HiFunSub ]
        , [ binaryN (string ">=")  HiFunNotLessThan
          , binaryN (string "<=")  HiFunNotGreaterThan
          , binaryN (do
            notFollowedBy (string "<=")
            (string "<"))  HiFunLessThan
          , binaryN (do
            notFollowedBy (string ">=")
            (string ">"))  HiFunGreaterThan
          , binaryN (string "==")  HiFunEquals
          , binaryN (string "/=") HiFunNotEquals]
        , [ binaryR (string "&&")  HiFunAnd]
        , [ binaryR (string "||")  HiFunOr]]

pHiOp :: Parser String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
pHiOp name f = ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ name)

binaryL :: Parser String -> HiFun -> Operator Parser HiExpr
binaryL name f = InfixL $ pHiOp name f

binaryR :: Parser String -> HiFun -> Operator Parser HiExpr
binaryR name f = InfixR $ pHiOp name f

binaryN :: Parser String -> HiFun -> Operator Parser HiExpr
binaryN name f = InfixN $ pHiOp name f

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (do
  sc
  res <- pHiExpr
  sc
  eof
  return res) "error"
