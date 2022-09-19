{- Reference: http://spec.graphql.org/draft/#sec-Int-Value -}

module IntegerParser where

import BaseParser
import NameParser
import Text.Megaparsec
import Text.Megaparsec.Char

readInt :: String -> Int
readInt = read

digit :: Parser Char
digit = oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

digitButNot0 :: Parser Char
digitButNot0 = satisfy (/= '0') *> digit

integerPart :: Parser Int
integerPart = do
  negativeSign <- try $ optional (char '-')
  firstDigit <- digit
  case firstDigit of
    '0' -> return 0
    _ -> do
      rest <- many digit
      let xInt = read (firstDigit : rest)
      case negativeSign of
        (Just '-') -> return $ xInt * (-1)
        _ -> return xInt

intValue :: Parser Int
intValue = integerPart <* notFollowedBy (char '.' <|> digit <|> nameStart)