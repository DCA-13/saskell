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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

posIntParser :: Parser Integer
posIntParser = lexeme L.decimal <?> "integer"

negIntParser :: Parser Integer
negIntParser = negate <$> (symbol "-" *> integerParser)

integerParser :: Parser Integer
integerParser = try negIntParser <|> posIntParser

posDoubleParser :: Parser Double
posDoubleParser = lexeme L.float <?> "double"

negDoubleParser :: Parser Double
negDoubleParser = negate <$> (symbol "-" *> doubleParser)

doubleParser :: Parser Double
doubleParser = try negDoubleParser <|> posDoubleParser

constParser :: Parser Expr
constParser = Const <$> (try doubleParser <|> (fromInteger <$> integerParser))

varParser :: Parser Expr
varParser = Var <$> (some (satisfy isAlphaNum) <* symbol "_") <?> "variable"

funParser :: Parser Expr
funParser = Fun <$> some (satisfy isAlphaNum) <*> parens exprParser <?> "function"

powParser :: Parser Expr
powParser = Pow <$> factorParser <*> (fromInteger <$> (symbol "^" *> (try integerParser <|> parens integerParser))) <?> "power"

factorParser :: Parser Expr
factorParser = choice (map try [constParser, varParser, funParser, parens exprParser]) <?> "factor"

handleList :: ([Expr] -> Expr) -> [Expr] -> Expr
handleList _ [] = Undefined
handleList _ [e] = e
handleList f es = f es

termParser :: Parser Expr
termParser = handleList Mul <$> listParser <?> "expression"
  where
    listParser = (:) <$> factorParser' <*> many (mulParser <|> divParser)
    factorParser' = try powParser <|> factorParser <?> "factor"
    mulParser = symbol "*" *> factorParser'
    divParser = symbol "/" *> (flip Pow (-1) <$> factorParser')

exprParser :: Parser Expr
exprParser = handleList Sum <$> listParser <?> "expression"
  where
    listParser = (:) <$> termParser <*> many (sumParser <|> subParser)
    sumParser = symbol "+" *> termParser
    subParser = symbol "-" *> ((Const (-1) :*) <$> termParser)

parser :: String -> Either (ParseErrorBundle String Void) Expr
parser = runParser (exprParser <* eof) ""
