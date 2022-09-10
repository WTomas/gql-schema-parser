module Main where

import Data.Functor
import Data.Maybe (isNothing)
import Data.Void (Void)
import GHC.Float (double2Float)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data GqlAtom
  = GqlNull
  | GqlString String
  | GqlInt Int
  | GqlFloat Float
  | GqlBool Bool
  deriving (Show)

data TypeDeclaration = Type Identifier Bool (Maybe GqlAtom) | ArrayType TypeDeclaration Bool (Maybe GqlAtom) deriving (Show)

newtype Identifier = Identifier String deriving (Show)

newtype PropertyKey = PropertyKey String deriving (Show)

newtype ArgKey = ArgKey String deriving (Show)

newtype Directive = Directive String deriving (Show)

newtype AtomType = AtomType String deriving (Show)

newtype GqlObjectProperty = GqlObjectProperty (PropertyKey, [(ArgKey, TypeDeclaration)], TypeDeclaration) deriving (Show)

data GqlExp
  = GqlEnum Identifier [PropertyKey]
  | GqlInterface Identifier [(PropertyKey, TypeDeclaration)]
  | GqlObject Identifier (Maybe Identifier) [GqlObjectProperty]
  | GqlUnion Identifier [Identifier]
  | GqlScaler Identifier
  | GqlDirective Directive [(ArgKey, TypeDeclaration)] [AtomType]
  deriving (Show)

type Parser = Parsec Void String

stringsToEscape :: [Char]
stringsToEscape = ['\\', '\"', '\'', '\n', '\t']

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf ['\\', '"', '\'', 'n', 't']
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\\', '\"', '\'', '\n', '\t']

stringP :: Parser GqlAtom
stringP = do
  let inner = return <$> try nonEscape <|> escape
   in do
        strings <- between (char '"') (char '"') (many inner)
        pure $ GqlString $ concat strings

nullP :: Parser GqlAtom
nullP = lexeme (string "null") $> GqlNull

boolP :: Parser GqlAtom
boolP = (GqlBool False <$ lexeme (string "false")) <|> (lexeme (string "true") $> GqlBool True)

gqlAtomP :: Parser GqlAtom
gqlAtomP =
  choice
    [ nullP,
      boolP,
      stringP
    ]

identifier :: Parser Identifier
identifier = do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  lexeme $ pure $ Identifier $ first : rest

propertyKey :: Parser PropertyKey
propertyKey = do
  (Identifier propKey) <- identifier
  pure $ PropertyKey propKey

typeP :: Parser TypeDeclaration
typeP = do
  typeName <- identifier
  exclamationMark <- optional $ lexeme (char '!')
  defaultValue <- optional $ lexeme (char '=') *> gqlAtomP
  case exclamationMark of
    Just _ -> pure $ Type typeName False defaultValue
    Nothing -> pure $ Type typeName True defaultValue

arrayTypeP :: Parser TypeDeclaration
arrayTypeP = do
  internalType <- lexeme (char '[') *> typeDeclarationP <* lexeme (char ']')
  exclamationMark <- optional $ lexeme (char '!')
  defaultValue <- optional $ lexeme (char '=') *> gqlAtomP
  pure $ ArrayType internalType (isNothing exclamationMark) defaultValue

typeDeclarationP :: Parser TypeDeclaration
typeDeclarationP = typeP <|> arrayTypeP

-- directiveP :: Parser Directive
-- directiveP = (lexeme (string "directive") *>)

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockCommentNested "\"\"\"" "\"\"\"")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

enumP :: Parser GqlExp
enumP = do
  enumName <- lexeme (string "enum") *> identifier <* lexeme (char '{')
  keys <- some (lexeme propertyKey) <* lexeme (char '}')
  pure $ GqlEnum enumName keys

propertyAndTypeP :: Parser (PropertyKey, TypeDeclaration)
propertyAndTypeP = do
  propertyName <- lexeme propertyKey <* lexeme (char ':')
  typeDeclaration <- lexeme typeDeclarationP
  pure (propertyName, typeDeclaration)

argAndTypeP :: Parser (ArgKey, TypeDeclaration)
argAndTypeP = do
  (PropertyKey argKey, typeDeclaration) <- propertyAndTypeP
  pure (ArgKey argKey, typeDeclaration)

objectPropertyP :: Parser GqlObjectProperty
objectPropertyP = do
  endpointName <- lexeme propertyKey
  args <- optional $ between (lexeme (char '(')) (lexeme (char ')')) ((:) <$> lexeme argAndTypeP <*> many (lexeme argAndTypeP))
  typeDeclaration <- lexeme (char ':') *> lexeme typeDeclarationP
  case args of
    Just _args -> pure $ GqlObjectProperty (endpointName, _args, typeDeclaration)
    Nothing -> pure $ GqlObjectProperty (endpointName, [], typeDeclaration)

interfaceP :: Parser GqlExp
interfaceP = do
  interfaceName <- lexeme (string "interface") *> identifier
  propertiesAndTypes <- between (lexeme (char '{')) (lexeme (char '}')) (many (lexeme propertyAndTypeP))
  pure $ GqlInterface interfaceName propertiesAndTypes

objectP :: Parser GqlExp
objectP = do
  constructName <- lexeme (string "type") *> identifier
  implementedInterface <- optional (lexeme (string "implements") *> identifier)
  propertiesAndTypes <- between (lexeme (char '{')) (lexeme (char '}')) (many (lexeme objectPropertyP))
  pure $ GqlObject constructName implementedInterface propertiesAndTypes

unionP :: Parser GqlExp
unionP = do
  unionName <- lexeme (string "union") *> identifier <* lexeme (char '=')
  unionMembers <- (:) <$> identifier <*> many (lexeme (char '|') *> identifier)
  pure $ GqlUnion unionName unionMembers

scalerP :: Parser GqlExp
scalerP = GqlScaler <$> (lexeme (string "scaler") *> identifier)

gqlExp :: Parser GqlExp
gqlExp =
  choice
    [ enumP,
      interfaceP,
      objectP,
      unionP,
      scalerP
    ]

main :: IO ()
main = do
  file <- readFile "enum-schema.gql"
  putStr file
  x <- parseTest objectPropertyP "filterFakeUsers: [Boolean] = true"
  putStr $ show x