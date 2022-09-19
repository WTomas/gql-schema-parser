module NameParser where

import BaseParser
import Text.Megaparsec
import Text.Megaparsec.Char

nameStart :: Parser Char
nameStart = letterChar <|> char '_'

name :: Parser String
name =
  (:)
    <$> nameStart
    <*> many (alphaNumChar <|> char '_')