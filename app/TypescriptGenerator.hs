{-# LANGUAGE LambdaCase #-}

module TypescriptGenerator where

import Parser

newtype Generator = Generator
  { runGenerator :: GqlDeclaration -> Maybe String
  }

indent :: Int -> String -> String
indent 0 s = s
indent nSpaces s = concatMap (const " ") [0 .. nSpaces - 1] ++ s

enumValueGenerator :: Generator
enumValueGenerator = Generator $ \case
  (GqlEnumDeclaration (Identifier identifier) keys) ->
    let lines =
          "class "
            ++ identifier
            ++ " {\n"
            ++ extraLines
            ++ "}\n"
     in Just lines
    where
      extraLines = concatMap (\(PropertyKey propertyKey) -> indent 2 "public static readonly " ++ propertyKey ++ ": any = '" ++ propertyKey ++ "';\n") keys
  _ -> Nothing

mockEnumValue :: GqlDeclaration
mockEnumValue = GqlEnumDeclaration (Identifier "MockEnum") [PropertyKey "KEY_A", PropertyKey "KEY_B"]

main :: IO ()
main = do
  let (Just s) = runGenerator enumValueGenerator mockEnumValue
  writeFile "generated.ts" s