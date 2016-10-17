{-# LANGUAGE DataKinds #-}
module Main where

import Control.Applicative (empty, liftA)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Lib

data Term =
    Leaf String
  | Method Term [Term] [Term]          -- a method with an identifier, list of params, list of statements
  | Identifier String String [Integer]
  | Indexed [Term]
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

pRange :: Parser [Integer]
pRange = sepBy1 L.integer (symbol ",")

-- false
pLiteral :: Parser Term
pLiteral = do
  value <- pItem
  return $ Leaf value

-- "hello"
pString :: Parser Term
pString = Leaf <$> quotes pItem

-- [:@ident, "hello", [1, 4]]
pIdent :: Parser Term
pIdent = do
  symbol "[:@"
  name <- pItem <* symbol ","
  value <- quotes pItem <* symbol ","
  range <- brackets pRange
  symbol "]"
  return $ Identifier name value range

--  [:def, [:@ident, ...], [:params, ...], [:bodystmt, ...]]
pMethod :: Parser Term
pMethod = do
  symbol "[:def,"
  ident <- pIdent <* symbol ","
  params <- pArray' <* symbol ","
  stmts <- pArray'
  symbol "]"
  return $ Method ident params stmts

pArray :: Parser Term
pArray = Indexed <$> pArray'

pArray' :: Parser [Term]
pArray' = brackets (sepBy1 pTerm (symbol ","))

pTerm :: Parser Term
pTerm = pLiteral <|> pString <|> pIdent <|> pMethod <|> pArray

parser :: Parser Term
parser = pTerm <* eof

main :: IO ()
main = do
  parseTest parser "[:@ident, \"hello\", [1, 4]]"
  parseTest parser "[:string_content, [:@tstring_content, \"hey\", [1, 17]]]"
  -- parseTest parser "[:def, [:@ident, \"hello\", [1, 4]], [:@tstring_content, \"hey\", [1, 17]]]"
  -- parseTest parser "[:def,\n[:@ident, \"hello\", [1, 4]],\n[:params, nil, nil, nil, nil, nil, nil, nil]]"
  -- parseTest parser "[:program, [[:def, [:@ident, \"hello\", [1, 4]]]]]"
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
