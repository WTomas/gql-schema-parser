module EnumParser where

import BaseParser
import NameParser
import qualified NullParser as N
import Text.Megaparsec
import Text.Megaparsec.Char

enum :: Parser String
enum = do
  parsed <- name
  case parsed of
    "true" -> empty
    "false" -> empty
    "null" -> empty
    _ -> pure parsed