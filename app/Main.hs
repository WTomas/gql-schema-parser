module Main where

import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Maybe (isNothing)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data GqlAtom
  = GqlNull
  | GqlString String
  | GqlInt Int
  | GqlFloat Float -- TODO
  | GqlBool Bool
  | GqlObject [(ArgKey, GqlAtom)] -- TODO
  | GqlArray [GqlAtom] -- TODO
  | GqlEnumValue Identifier
  deriving (Show)

data TypeDeclaration = Type Identifier Bool (Maybe GqlAtom) | ArrayType TypeDeclaration Bool (Maybe GqlAtom) deriving (Show)

newtype Identifier = Identifier String deriving (Show)

newtype PropertyKey = PropertyKey String deriving (Show)

newtype ConstructType = ConstructType String deriving (Show)

newtype ArgKey = ArgKey String deriving (Show)

newtype Directive = Directive String deriving (Show)

newtype DirectiveCall = DirectiveCall (Directive, [(ArgKey, GqlAtom)]) deriving (Show)

newtype GqlObjectProperty = GqlObjectProperty (PropertyKey, [(ArgKey, TypeDeclaration)], TypeDeclaration) deriving (Show)

data GqlDeclaration
  = GqlEnumDeclaration Identifier [PropertyKey]
  | GqlInterfaceDeclaration Identifier [(PropertyKey, TypeDeclaration)]
  | GqlInputDeclaration Identifier [(PropertyKey, TypeDeclaration)]
  | GqlObjectDeclaration Identifier (Maybe Identifier) [GqlObjectProperty]
  | GqlUnionDeclaration Identifier [Identifier]
  | GqlScalerDeclaration Identifier (Maybe DirectiveCall)
  | GqlDirectiveDeclaration Directive [(ArgKey, TypeDeclaration)] [ConstructType]
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

enumValueP :: Parser GqlAtom
enumValueP = GqlEnumValue <$> identifier

intP :: Parser GqlAtom
intP = GqlInt . read <$> some (satisfy isDigit)

gqlAtomP :: Parser GqlAtom
gqlAtomP =
  choice
    [ nullP,
      boolP,
      stringP,
      enumValueP,
      intP
    ]

varname :: Parser String
varname = do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  lexeme $ pure $ first : rest

identifier :: Parser Identifier
identifier = Identifier <$> varname

propertyKey :: Parser PropertyKey
propertyKey = PropertyKey <$> varname

argKey :: Parser ArgKey
argKey = ArgKey <$> varname

constructType :: Parser ConstructType
constructType = ConstructType <$> varname

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

directiveP :: Parser Directive
directiveP = Directive <$> (char '@' *> varname)

directiveDeclarationP :: Parser GqlDeclaration
directiveDeclarationP = do
  directive <- lexeme (string "directive") *> directiveP
  args <- optional $ between (lexeme (char '(')) (lexeme (char ')')) $ lexeme ((:) <$> argAndTypeP <*> many argAndTypeP)
  actingOn <- lexeme (string "on") *> sepBy constructType (lexeme (char '|'))
  case args of
    Just _args -> pure $ GqlDirectiveDeclaration directive _args actingOn
    Nothing -> pure $ GqlDirectiveDeclaration directive [] actingOn

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockCommentNested "\"\"\"" "\"\"\"")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

propertyAndTypeP :: Parser (PropertyKey, TypeDeclaration)
propertyAndTypeP = do
  prop <- lexeme propertyKey
  typeDeclaration <- lexeme (char ':') *> lexeme typeDeclarationP
  pure (prop, typeDeclaration)

argAndTypeP :: Parser (ArgKey, TypeDeclaration)
argAndTypeP = do
  arg <- lexeme argKey
  typeDeclaration <- lexeme (char ':') *> lexeme typeDeclarationP <* optional (char ',')
  pure (arg, typeDeclaration)

argAndAtomP :: Parser (ArgKey, GqlAtom)
argAndAtomP = do
  arg <- lexeme argKey
  atom <- lexeme (char ':') *> lexeme gqlAtomP
  pure (arg, atom)

objectPropertyP :: Parser GqlObjectProperty
objectPropertyP = do
  endpointName <- lexeme propertyKey
  args <- optional $ between (lexeme (char '(')) (lexeme (char ')')) ((:) <$> lexeme argAndTypeP <*> many (lexeme argAndTypeP))
  typeDeclaration <- lexeme (char ':') *> lexeme typeDeclarationP
  case args of
    Just _args -> pure $ GqlObjectProperty (endpointName, _args, typeDeclaration)
    Nothing -> pure $ GqlObjectProperty (endpointName, [], typeDeclaration)

{- Declaration Parsers -}
enumDecP :: Parser GqlDeclaration
enumDecP = do
  enumName <- lexeme (string "enum") *> identifier
  keys <- between (lexeme (char '{')) (lexeme (char '}')) (some (lexeme propertyKey))
  pure $ GqlEnumDeclaration enumName keys

interfaceOrInputDecP :: String -> Parser GqlDeclaration
interfaceOrInputDecP which = do
  interfaceName <- lexeme (string which) *> identifier
  propertiesAndTypes <- between (lexeme (char '{')) (lexeme (char '}')) (many (lexeme propertyAndTypeP))
  if which == "interface"
    then pure $ GqlInterfaceDeclaration interfaceName propertiesAndTypes
    else pure $ GqlInputDeclaration interfaceName propertiesAndTypes

objectDecP :: Parser GqlDeclaration
objectDecP = do
  constructName <- lexeme (string "type") *> identifier
  implementedInterface <- optional (lexeme (string "implements") *> identifier)
  propertiesAndTypes <- between (lexeme (char '{')) (lexeme (char '}')) (many (lexeme objectPropertyP))
  pure $ GqlObjectDeclaration constructName implementedInterface propertiesAndTypes

unionDecP :: Parser GqlDeclaration
unionDecP = do
  unionName <- lexeme (string "union") *> identifier <* lexeme (char '=')
  unionMembers <- (:) <$> identifier <*> many (lexeme (char '|') *> identifier)
  pure $ GqlUnionDeclaration unionName unionMembers

directiveCallP :: Parser DirectiveCall
directiveCallP = do
  directive <- directiveP
  args <- optional $ between (lexeme (char '(')) (lexeme (char ')')) $ many argAndAtomP
  case args of
    (Just _args) -> pure $ DirectiveCall (directive, _args)
    Nothing -> pure $ DirectiveCall (directive, [])

scalerDecP :: Parser GqlDeclaration
scalerDecP = do
  scalar <- lexeme (string "scalar") *> lexeme identifier
  directiveCall <- optional directiveCallP
  pure $ GqlScalerDeclaration scalar directiveCall

gqlDeclaration :: Parser GqlDeclaration
gqlDeclaration =
  choice
    [ enumDecP,
      interfaceOrInputDecP "interface",
      interfaceOrInputDecP "input",
      objectDecP,
      unionDecP,
      scalerDecP,
      directiveDeclarationP
    ]

main :: IO ()
main = do
  file <- readFile "enum-schema.gql"
  putStr file
  x <- parseTest (some gqlDeclaration) file
  putStr $ show x