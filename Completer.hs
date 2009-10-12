import Data.List (isPrefixOf)
import Parser (tokenize)

-- Example

git :: Completer
git = str "git" --> many gitOptions --> gitCommand

gitOptions :: Completer
gitOptions = str "--version" <|> str "--help" <|> str "--work-tree"

gitCommand :: Completer
gitCommand = (str "add" --> many (str "-i" <|> str "-n" <|> str "-v"))
         <|> (str "commit" --> many (str "-m" <|> str "-a" <|> str "--amend"))

-- Primitives

data Completion = Tokens [String] | Suggestions [String]
type Completer = [String] -> [Completion]

run :: Completer -> String -> [String]
run c s = [s | Suggestions xs <- c (tokenize s), s <- xs]

continue :: Completer
continue ts = [Tokens ts]

(<|>) :: Completer -> Completer -> Completer
c <|> d = \ts -> c ts ++ d ts

(-->) :: Completer -> Completer -> Completer
c --> d = \ts -> concat [ case result of
                            Tokens ts' -> d ts'
                            _          -> [result]
                        | result <- c ts]

-- Matching

str :: String -> Completer
str s = match (s ==) (\t -> Suggestions $ filter (t `isPrefixOf`) [s])

match :: (String -> Bool) -> (String -> Completion) -> Completer
match p suggest ts = case ts of
    []     -> []
    [t]    -> [suggest t]
    (t:ts) -> if p t then continue ts else []

-- Repetition

many :: Completer -> Completer
many p = many1 p <|> continue

many1 :: Completer -> Completer
many1 p = p --> (many p <|> continue)
