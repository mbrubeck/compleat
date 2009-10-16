module Tokenize () where

import Text.Parsec.Char (anyChar, char, noneOf, space)
import Text.Parsec.Combinator (between, eof, many1, manyTill)
import Text.Parsec.Prim ((<|>), many, try, runParser)
import Text.Parsec.String (Parser)

tokens :: Parser [String]
tokens = many token

token = try (many space >> (quoted '"' <|> quoted '\'' <|> unquoted))
    <|> (many1 space >> eof >> return "")

unquoted :: Parser String
unquoted = manyTill (escaped <|> anyChar) (try space)

quoted :: Char -> Parser String
quoted q = between (char q) (char q <|> return '_') (many nonQuote)
    where nonQuoteChar = escaped <|> noneOf [q]

escaped :: Parser Char
escaped = do
    char '\\'
    c <- anyChar
    case c of
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        _   -> return c
