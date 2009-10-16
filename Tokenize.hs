module Tokenize () where

import Text.Parsec.Char (anyChar, char, noneOf, space, spaces)
import Text.Parsec.Combinator (between, eof, many1, manyTill)
import Text.Parsec.Prim ((<|>), many, try, runParser)
import Text.Parsec.String (Parser)

tokens :: Parser [String]
tokens = many token

token = spaces >> (quoted '"' <|> quoted '\'' <|> unquoted)

quoted :: Char -> Parser String
quoted q = do
    char q
    manyTill (escaped <|> anyChar) (try (char q >> return ()) <|> eof)

unquoted :: Parser String
unquoted = do
    c <- anyChar
    cs <- manyTill (escaped <|> anyChar) (try (space >> return ()) <|> eof)
    return (c:cs)

escaped :: Parser Char
escaped = do
    char '\\'
    c <- anyChar
    case c of
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        _   -> return c
