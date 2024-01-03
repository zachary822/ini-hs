{-# LANGUAGE OverloadedStrings #-}

module Data.Ini where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

newtype Header = Header Text deriving (Eq, Ord, Show, IsString)

type Name = Text
type Value = Text
type Assignments = Map Name Value

data Section = Section !Header !Assignments deriving (Eq, Ord, Show)

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space space1 skipLineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parseBracketPair :: Parser a -> Parser a
parseBracketPair = between (char '[') (char ']')

parseHeader :: Parser Header
parseHeader =
  lexeme
    ( parseBracketPair
        (Header . T.pack <$> many (choice [alphaNumChar, oneOf ['.', ' ']]))
        <?> "header"
    )

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- lexeme $ some alphaNumChar

  _ <- symbol "="

  value <- lexeme $ many alphaNumChar

  return (T.pack name, T.pack value)

parseAssignments :: Parser [(Name, Value)]
parseAssignments = many parseAssignment

parseSection :: Parser Section
parseSection = do
  sc
  h <- parseHeader
  Section h . M.fromList <$> parseAssignments

parseIni :: Parser [Section]
parseIni = do
  sc
  assignments <- parseAssignments
  sections <- many parseSection

  if null assignments
    then return sections
    else return $ Section "default" (M.fromList assignments) : sections
