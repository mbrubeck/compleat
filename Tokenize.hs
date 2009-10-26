module Tokenize (tokenize, tokens, token) where

import Data.Char (isSpace)
import Text.ParserCombinators.Parsec hiding (token, tokens)

tokenize :: String -> [String]
tokenize s = case runParser tokens () "" s of
                Right ts -> ts
                Left  _  -> []

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
unquoted = many1 (escaped <|> satisfy (not . isSpace))

escaped :: Parser Char
escaped = do
    char '\\'
    c <- anyChar
    case c of
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        _   -> return c
