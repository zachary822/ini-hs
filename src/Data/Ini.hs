{-# LANGUAGE OverloadedStrings #-}

module Data.Ini where

import Data.Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.String
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void TL.Text

newtype Header = Header T.Text deriving (Eq, Ord, Show, IsString)

type Name = T.Text
type Value = T.Text
type Assignments = Map Name Value

type Ini = Map Header (Map Name Value)

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space space1 skipLineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: TL.Text -> Parser TL.Text
symbol = L.symbol sc

parseBracketPair :: Parser a -> Parser a
parseBracketPair = between (char '[') (char ']')

isHeader :: Char -> Bool
isHeader c = isAlphaNum c || (c `elem` ['.', ' '])

parseHeader :: Parser Header
parseHeader =
  lexeme
    ( parseBracketPair
        (Header . TL.toStrict <$> takeWhileP (Just "header") isHeader)
    )

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- lexeme $ takeWhile1P (Just "name") isAlphaNum

  _ <- symbol "="

  value <- lexeme $ takeWhileP (Just "value") (`notElem` ['\n', '\r', ';'])

  return (TL.toStrict name, T.strip $ TL.toStrict value)

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

parseIni :: TL.Text -> Either (ParseErrorBundle TL.Text Void) Ini
parseIni = runParser parseIni' ""

parseIniFile :: FilePath -> IO (Either (ParseErrorBundle TL.Text Void) Ini)
parseIniFile f = do
  c <- TIO.readFile f
  return $ runParser parseIni' f c
