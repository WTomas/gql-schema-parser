module StringParser where

import BaseParser
import Text.Megaparsec
import Text.Megaparsec.Char

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u']
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\"', '\\', '/', '\b', '\f', '\n', '\r', '\t']

blockStringValue :: Parser String
blockStringValue = do
  let inner = return <$> try nonEscape <|> escape
   in do
        strings <- between (string "\"\"\"") (string "\"\"\"") (many (lexeme inner))
        pure $ concat strings

regularStringValue :: Parser String
regularStringValue = do
  let inner = return <$> try nonEscape <|> escape
   in do
        strings <- between (char '"') (char '"') (many inner)
        pure $ concat strings

stringValue :: Parser String
stringValue = blockStringValue <|> regularStringValue