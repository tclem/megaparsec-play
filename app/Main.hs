{-# LANGUAGE DataKinds #-}
module Main where

import Control.Applicative (empty, liftA)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Lib

data Term = Leaf String String [Integer]
          | Params String [String]
          | Block String [Term] String
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

doubleBrackets :: Parser a -> Parser a
doubleBrackets = between (symbol "[[") (symbol "]]")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

pItem :: Parser String
pItem = lexeme $ some (alphaNumChar <|> char ':' <|> char '@' <|> char '_')

pRange :: Parser [Integer]
pRange = sepBy1 L.integer (symbol ",")

pValueRange :: String -> Parser Term
pValueRange name = do
  value <- quotes pItem <* lexeme (symbol ",")
  range <- brackets pRange
  return $ Leaf name value range

pNestedTerm :: String -> Parser Term
pNestedTerm name = do
  next <- pTerms
  return $ Indexed name next

pParamsTerm :: String -> Parser Term
pParamsTerm name = do
  params <- sepBy1 pItem (symbol ",")
  return $ Params name params

pName :: Parser Term
pName = do
  value <- pItem
  return $ Leaf "" value []

pBlockTerm :: String -> Parser Term
pBlockTerm name = do
  next <- pTerms <* symbol ","
  ret <- pItem
  return $ Block name next ret

pTerm :: Parser Term
pTerm = doubleBrackets go <|> brackets go <|> pName
  where
    go = do
      name <- pItem <* lexeme (symbol ",")
      pValueRange name <|> pNestedTerm name <|> pParamsTerm name

pTerms :: Parser [Term]
pTerms = sepBy1 pTerm (symbol ",")

parser :: Parser Term
parser = pTerm <* eof

main :: IO ()
main = do
  parseTest parser "[:@ident, \"hello\", [1, 4]]"
  parseTest parser "[:string_content, [:@tstring_content, \"hey\", [1, 17]]]"
  parseTest parser "[:def, [:@ident, \"hello\", [1, 4]], [:@tstring_content, \"hey\", [1, 17]]]"
  parseTest parser "[:def,\n[:@ident, \"hello\", [1, 4]],\n[:params, nil, nil, nil, nil, nil, nil, nil]]"
  parseTest parser "[:program, [[:def, [:@ident, \"hello\", [1, 4]]]]]"
  parseTest parser program


-- [:program,
--  [[:def,
--    [:@ident, "hello", [1, 4]],
--    [:params, nil, nil, nil, nil, nil, nil, nil],
--    [:bodystmt,
--     [[:command,
--       [:@ident, "puts", [1, 11]],
--       [:args_add_block,
--        [[:string_literal,
--          [:string_content, [:@tstring_content, "hey", [1, 17]]]]],
--        false]]],
--     nil,
--     nil,
--     nil]]]]
program :: String
program = "[:program,\
\   [[:def,\
\     [:@ident, \"hello\", [1, 4]],\
\     [:params, nil, nil, nil, nil, nil, nil, nil],\
\     [:bodystmt,\
\      [[:command,\
\        [:@ident, \"puts\", [1, 11]],\
\        [:args_add_block,\
\         [[:string_literal,\
\           [:string_content, [:@tstring_content, \"hey\", [1, 17]]]]],\
\         false]]],\
\      nil,\
\      nil,\
\      nil]]]]"
