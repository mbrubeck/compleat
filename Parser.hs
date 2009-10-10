module Parser
    ( Parser, parse, getInput
    , (<|>), (+++)
    , item, sat, char
    , spaces, nonspace
    , many, many1, many'
    , bracket
    ) where

import Control.Monad (MonadPlus, mzero, mplus, guard)
import Data.Char (isSpace)

data Parser a = Parser { parse :: (String -> [(a, String)]) }

instance Monad Parser where
    return a  = Parser (\cs -> [(a, cs)])
    p >>= f   = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadPlus Parser where
    mzero     = Parser (\cs -> [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                            []     -> []
                            (x:xs) -> [x])

getInput :: Parser String
getInput = Parser (\cs -> [(cs, cs)])

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

spaces :: Parser String
spaces = many (sat isSpace)

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

split :: Parser [a] -> Parser a
split p = Parser (\cs -> [(a,cs') | (as, cs') <- parse p cs,
                                    not (null as),
                                    let a = last as])

many' :: Parser a -> Parser a
many' p = split $ many1 p `mplus` return []

many1' :: Parser a -> Parser a
many1' = split . many1

-- Expressions

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket start middle end = do
    start
    result <- middle
    end
    return result
