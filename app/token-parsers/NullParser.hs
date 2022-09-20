module NullParser where

import BaseParser
import Text.Megaparsec.Char

null :: Parser String
null = string "null"