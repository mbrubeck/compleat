module Parser
    ( Parser, parse, run
    , getInput, setInput
    , (<|>), (+++)
    , item, sat, char, string
    , space, nonspace
    , many, many1
    , bracket
    , token, tokens, tokenize
    ) where

import Control.Monad (MonadPlus, mzero, mplus, guard)
import Data.Char (isSpace)

data Parser a = Parser { parse :: (String -> [(a, String)]) }

run :: Parser a -> String -> [a]
run p s = map fst $ parse p s

instance Monad Parser where
    return a  = Parser (\cs -> [(a, cs)])
    p >>= f   = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadPlus Parser where
    mzero     = Parser (\_  -> [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                            []     -> []
                            (x:xs) -> [x])

getInput :: Parser String
getInput = Parser (\cs -> [(cs, cs)])

setInput :: String -> Parser String
setInput inp = Parser (\cs -> [(cs, inp)])

eof :: Parser ()
eof = Parser (\cs -> case cs of
                        [] -> [((), cs)]
                        _  -> [])

-- Characters

item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do 
    x <- item
    guard (p x)
    return x

char :: Char -> Parser Char
char c = sat (c==)

string :: String -> Parser String
string ""     = return ""
string (x:xs) = char x >> string xs >> return (x:xs)

space :: Parser Char
space = sat isSpace

nonspace :: Parser Char
nonspace = sat (not . isSpace)

-- Repetition

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
    a <- p
    as <- many p
    return (a:as)

-- Expressions

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket start middle end = do
    start
    result <- middle
    end
    return result

-- Tokenize

tokenize :: String -> [String]
tokenize s = head (run tokens s)

tokens :: Parser [String]
tokens = apply (many token)

apply :: Parser a -> Parser a
apply p = p

token :: Parser String
token = (many space >> (quotedToken '"' +++ quotedToken '\'' +++ unquotedToken))
    <|> (many1 space >> eof >> return "")

unquotedToken :: Parser String
unquotedToken = many1 (escaped +++ nonspace)

quotedToken :: Char -> Parser String
quotedToken q = bracket (char q) (many nonQuoteChar) (char q +++ return '_')
    where nonQuoteChar = escaped +++ sat (q/=)

escaped :: Parser Char
escaped = do
    char '\\'
    c <- item
    case c of
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        _   -> return c
