module Completer
    ( Completer, run
    , continue, optional, skip
    , (<|>), (-->)
    , str
    , many, many1
    ) where
import Data.List (isPrefixOf)
import Tokenize (tokenize)

type Completer = [String] -> [Completion]
data Completion = Tokens [String] | Suggestions [String]

run :: Completer -> String -> [String]
run c s = [s | Suggestions xs <- c (tokenize s), s <- xs]

-- Matching

str :: String -> Completer
str s = match (s ==) (\t -> Suggestions $ filter (t `isPrefixOf`) [s])

match :: (String -> Bool) -> (String -> Completion) -> Completer
match p suggest ts = case ts of
    []     -> []
    [t]    -> [suggest t]
    (t:ts) -> if p t then continue ts else []

-- Primitives

continue :: Completer
continue ts = [Tokens ts]

skip :: Completer
skip (t:ts) = [Tokens ts]
skip _      = []

-- Combinators

optional :: Completer -> Completer
optional c = c <|> continue

(<|>) :: Completer -> Completer -> Completer
c <|> d = \ts -> c ts ++ d ts

(-->) :: Completer -> Completer -> Completer
c --> d = \ts -> concat [ case result of
                            Tokens ts' -> d ts'
                            _          -> [result]
                        | result <- c ts]

many :: Completer -> Completer
many p = many1 p <|> continue

many1 :: Completer -> Completer
many1 p = p --> many p
