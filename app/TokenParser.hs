module TokenParser where

import BaseParser
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Monoid (Any)
import Data.String
import Data.Void (Void)
import FloatParser
import IntegerParser
import NameParser
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data GqlToken
  = Punctuator String
  | Name String
  | IntValue Int
  | FloatValue Float
  | StringValue String
  deriving (Show)

data GqlValue
  = Variable GqlToken
  deriving (Show)

newtype Description = Description String deriving (Show)

newtype Directive = Directive String deriving (Show)

data TypeDefinition
  = ScalarTypeDefinition (Maybe Description) GqlToken (Maybe [Directive])
  deriving (Show)

punctuator :: Parser GqlToken
punctuator =
  Punctuator
    <$> choice
      [ string "!",
        string "$",
        string "(",
        string ")",
        string "...",
        string ":",
        string "=",
        string "@",
        string "[",
        string "]",
        string "{",
        string "}",
        string "|"
      ]

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf ['\\', '"', '\'', 'n', 't']
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\\', '\"', '\'', '\n', '\t']

stringValue :: Parser GqlToken
stringValue = do
  let inner = return <$> try nonEscape <|> escape
   in do
        strings <- between (char '"') (char '"') (many inner)
        pure $ StringValue $ concat strings

gqlToken :: Parser GqlToken
gqlToken =
  choice
    [ punctuator,
      Name <$> name,
      FloatValue <$> floatValue,
      IntValue <$> intValue,
      stringValue
    ]

description :: Parser Description
description = do
  let inner = return <$> try nonEscape <|> escape
   in do
        strings <- between (lexeme (string "\"\"\"")) (lexeme (string "\"\"\"")) (lexeme (many inner))
        pure $ Description $ concat strings

main :: IO ()
main = do
  parsed <- parseTest (some (description)) "\"\"\"SomeDescription\"\"\""
  putStr $ show parsed