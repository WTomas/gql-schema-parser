module BooleanParser where

import BaseParser
import Data.Functor (($>), (<$))
import Text.Megaparsec
import Text.Megaparsec.Char

boolean :: Parser Bool
boolean = True <$ string "true" <|> string "false" $> False