module Tokenize () where

import Text.Parsec.Char (anyChar, char, noneOf, space, spaces)
import Text.Parsec.Combinator (between, eof, many1, manyTill, option)
import Text.Parsec.Prim ((<|>), many, try, runParser)
import Text.Parsec.String (Parser)

tokens :: Parser [String]
tokens = do
    ts <- many (try token)
    last <- option [] (many1 space >> return [""])
    return (ts ++ last)

token = spaces >> (quoted '"' <|> quoted '\'' <|> unquoted)

quoted :: Char -> Parser String
quoted q = do
    char q
    manyTill (escaped <|> anyChar) (try (char q >> return ()) <|> eof)

unquoted :: Parser String
unquoted = many1 (escaped <|> noneOf " ")

escaped :: Parser Char
escaped = do
    char '\\'
    c <- anyChar
    case c of
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        _   -> return c
