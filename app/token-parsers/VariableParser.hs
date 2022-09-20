module VariableParser where

import BaseParser
import NameParser
import Text.Megaparsec
import Text.Megaparsec.Char

variableParser :: Parser String
variableParser = (:) <$> char '$' <*> name