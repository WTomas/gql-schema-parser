module BaseParser where

import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

unicodeBOM :: Parser Char
unicodeBOM = char (toEnum 0xFEFF)

spaceOrTab :: Parser String
spaceOrTab = some (char (toEnum 0x0009) <|> char (toEnum 0x0020))

lineTerminator :: Parser String
lineTerminator = some (char (toEnum 0x000A) <|> char (toEnum 0x000D))

skip :: Parser a -> Parser ()
skip p = p $> ()

skippable :: Parser ()
skippable =
  L.space
    ( skip spaceOrTab
        <|> skip lineTerminator
        <|> skip (char ',')
        <|> skip unicodeBOM
    )
    (L.skipLineComment "#")
    (L.skipLineComment "#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skippable