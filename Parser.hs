-- {-# LANGUAGE OverloadedStrings #-}

module Parser (parser) where

import Data.Char
import Data.Void
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")

posIntParser :: Parser Integer
posIntParser = lexeme L.decimal <?> "integer"

negIntParser :: Parser Integer
negIntParser = negate <$> (symbol "-" *> integerParser)

integerParser :: Parser Integer
integerParser = negIntParser <|> posIntParser

doubleParser :: Parser Double
doubleParser = lexeme L.float <?> "double"

constParser :: Parser Expr
constParser = Const <$> (try doubleParser <|> (fromInteger <$> integerParser))

varParser :: Parser Expr
varParser = Var <$> (some (satisfy isAlphaNum) <* symbol "_") <?> "variable"

funParser :: Parser Expr
funParser = Fun <$> some (satisfy isAlphaNum) <*> parens exprParser <?> "function"

powParser :: Parser Expr
powParser = Pow <$> factorParser <*> (fromInteger <$> (symbol "^" *> integerParser)) <?> "power"

factorParser :: Parser Expr
factorParser = choice (map try [constParser, varParser, funParser, parens exprParser]) <?> "factor"

ifSingle _ [e] = e
ifSingle f es = f es

-- TODO: division
termParser :: Parser Expr
termParser = ifSingle Mul <$> (try powParser <|> factorParser) `sepBy` symbol "*" <?> "term"

-- TODO: subtraction
exprParser :: Parser Expr
exprParser = ifSingle Sum <$> termParser `sepBy` symbol "+" <?> "expression"

runMyParser :: Parser a -> String -> Either String a
runMyParser parser input =
  case runParser parser "" input of
    Left err -> Left $ errorBundlePretty err
    Right x  -> Right x

parser = runParser exprParser ""
