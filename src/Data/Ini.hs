{-# LANGUAGE OverloadedStrings #-}

module Data.Ini where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.String
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

newtype Header = Header Text deriving (Eq, Ord, Show, IsString)

type Name = Text
type Value = Text
type Assignments = Map Name Value

type Ini = Map Header (Map Name Value)

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

parseSection :: Parser (Header, Assignments)
parseSection = do
  sc
  h <- parseHeader
  (h,) . M.fromList <$> parseAssignments

parseIni' :: Parser Ini
parseIni' = do
  sc
  assignments <- parseAssignments
  sections <- many parseSection

  return . M.fromList $
    if null assignments
      then sections
      else ("default", M.fromList assignments) : sections

parseIni :: Text -> Either (ParseErrorBundle Text Void) Ini
parseIni = runParser parseIni' ""

parseIniFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Ini)
parseIniFile f = do
  c <- TIO.readFile f
  return $ runParser parseIni' f c
