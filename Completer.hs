module Completer
    ( Completer, run
    , continue, optional, skip
    , (<|>), (-->)
    , str, file
    , many, many1
    ) where
import Control.Monad (liftM, sequence)
import Data.List (isPrefixOf)
import System.Directory (getDirectoryContents)

-- The "Completer" type is a function that takes a list of input tokens and
-- returns a list of possible completions.  Each completion can be a new list
-- of tokens (which will be handed to the next completer in a sequence) or
-- an action to generate a list of strings.
--
-- This module provides a set of primitive completers, and combinators to build
-- new completers through alternation, repetition, and sequencing.  These are
-- essentially identical to the parser combinators in "Monadic Parsing" by Hutton
-- and Meijer, except that they short-circuit: if all of the input is consumed by
-- one completer in a sequence, then the sequence returns the results of that
-- completer, rather than failure.

type Completer = [String] -> [Completion]
data Completion = Tokens [String] | Suggestions (IO [String])

run :: Completer -> [String] -> IO [String]
run c ts = liftM concat $ sequence [x | Suggestions x <- c ts]


-- Matching

-- Match or suggest the specified string.
str :: String -> Completer
str s = match (s ==) (\t -> if t `isPrefixOf` s then return [s ++ " "] else return [])

-- Match anything, or suggest a list of matching files.
-- (This is not yet fully implemented - it only looks for files in the current directory.)
file :: Completer
file = match (const True) (\t -> liftM (filter (t `isPrefixOf`)) (getDirectoryContents "."))

-- Build a completer that looks at a single token. If more input remains then call
-- predicate "p" to decide whether to continue.  Otherwise, call fuction "suggest"
-- to generate a list of completions.
match :: (String -> Bool) -> (String -> IO [String]) -> Completer
match p suggest ts = case ts of
    []     -> []
    [t]    -> [Suggestions $ suggest t]
    (t:ts) -> if p t then continue ts else []


-- Other primitives

-- Consume nothing.
continue :: Completer
continue ts = [Tokens ts]

-- Consume anything.
skip :: Completer
skip (t:ts) = [Tokens ts]
skip _      = []


-- Combinators

optional :: Completer -> Completer
optional c = c <|> continue

-- Choice operator.
(<|>) :: Completer -> Completer -> Completer
c <|> d = \ts -> c ts ++ d ts

-- Sequence operator.
(-->) :: Completer -> Completer -> Completer
c --> d = \ts -> concat [ case result of
                            Tokens ts' -> d ts'
                            _          -> [result]
                        | result <- c ts]

-- Repetition.
many :: Completer -> Completer
many p = many1 p <|> continue

many1 :: Completer -> Completer
many1 p = p --> many p
