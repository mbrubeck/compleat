module Tokenize (tokenize, tokens, token) where

import Data.Char (isSpace)
import Text.ParserCombinators.Parsec hiding (token, tokens)

-- | @tokenize@
-- Split a shell command into a list of words, attempting to use the same
-- rules as the shell (does not yet handle certain cases like subshells).
-- The command might be incomplete, so handle unbalanced quotes, and treat
-- trailing whitespace as the start of an empty token.

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
