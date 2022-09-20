module FloatParser where

import BaseParser
import IntegerParser
import NameParser (nameStart)
import Text.Megaparsec
import Text.Megaparsec.Char

exponentIndicator :: Parser Char
exponentIndicator = oneOf ['e', 'E']

sign :: Parser Char
sign = oneOf ['-', '+']

exponentPart :: Parser String
exponentPart = do
  e <- exponentIndicator
  s <- try (optional sign)
  rest <- some digit
  case s of
    Nothing -> return (e : rest)
    (Just sign') -> return (e : sign' : rest)

fractionalPart :: Parser String
fractionalPart = (:) <$> char '.' <*> some digit

readFloat :: String -> Float
readFloat = read

floatPart :: Parser Float
floatPart = do
  iPart <- integerPart
  fPart <- try (optional fractionalPart)
  ePart <- try (optional exponentPart)
  case (fPart, ePart) of
    (Nothing, Nothing) -> empty
    (Just fPart', Nothing) -> return $ readFloat (show iPart ++ fPart')
    (Nothing, Just ePart') -> return $ readFloat (show iPart ++ ePart')
    (Just fPart', Just ePart') -> return $ readFloat (show iPart ++ fPart' ++ ePart')

floatValue :: Parser Float
floatValue = floatPart <* notFollowedBy (char '.' <|> digit <|> nameStart)
