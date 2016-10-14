{-# LANGUAGE DataKinds #-}
module Main where

import Control.Applicative (empty, liftA)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Lib

-- type Range = [Integer]
-- data Term l = Term (Syntax l) Range
-- data Syntax a = Leaf a
--               | Indexed [Syntax a]
--               deriving (Eq, Show)

data Term = Leaf String String [Integer]
          | Indexed String [Term]
          deriving(Eq, Show)


sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

pItem :: Parser String
pItem = lexeme $ some (alphaNumChar <|> char ':' <|> char '@' <|> char '_')

-- pItem' :: Parser Term
-- pItem' = liftA Leaf (lexeme $ some (alphaNumChar <|> char ':' <|> char '@'))

pRange :: Parser [Integer]
pRange = sepBy1 L.integer (symbol ",")

pValueRange' :: String -> Parser Term
pValueRange' name = do
  value <- quotes pItem
  _ <- lexeme (symbol ",")
  range <- brackets pRange
  return $ Leaf name value range

pNestedTerm' :: String -> Parser Term
pNestedTerm' name = do
  next <- brackets pTerm'
  return $ Indexed name [next]

pTerm' :: Parser Term
pTerm' = do
  name <- pItem
  _ <- lexeme (symbol ",")
  pValueRange' name <|> pNestedTerm' name

pTerm :: Parser (String, String, [Integer])
pTerm = do
  name <- pItem
  _ <- lexeme (symbol ",")
  value <- quotes pItem
  _ <- lexeme (symbol ",")
  range <- brackets pRange
  return (name, value, range)

parser :: Parser (String, String, [Integer])
parser = brackets pTerm <* eof

parser' :: Parser Term
parser' = brackets pTerm' <* eof

main :: IO ()
-- [:@ident, "hello", [1, 4]]
-- main = parseTest parser "[:@ident, \"hello\", [1, 4]]"
-- [:string_content, [:@tstring_content, "hey", [1, 17]]]
-- main = parseTest parser' "[:@tstring_content, \"hey\", [1, 17]]"
main = parseTest parser' "[:string_content, [:@tstring_content, \"hey\", [1, 17]]]"
