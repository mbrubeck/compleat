module Completer
    ( Completer, run
    , continue, optional, skip
    , (<|>), (-->)
    , str, shellCommand
    , many, many1
    ) where
import Control.Monad (liftM, sequence)
import Data.List (isPrefixOf)
import IO (hGetContents)
import System.Directory (getDirectoryContents)
import System.Posix.Env (setEnv)
import System.Process (StdStream(CreatePipe), createProcess, proc, std_out)

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

-- Shell commands

shellCommand :: String -> Completer
shellCommand command = match (const True) (\t -> do
    setEnv "COMP_CWORD" t True
    output <- getCommandOutput command
    return $ matchesFrom (lines output) t)

getCommandOutput :: String -> IO String
getCommandOutput command = do
    (_, Just hout, _, _) <- createProcess (proc "bash" ["-c", command])
                                          {std_out = CreatePipe}
    hGetContents hout

-- Matching

-- Match or suggest the specified string.
str :: String -> Completer
str s = match (s ==) (\t -> return $ matchesFrom [s] t)

-- Build a completer that looks at a single token. If more input remains then call
-- predicate "p" to decide whether to continue.  Otherwise, call fuction "suggest"
-- to generate a list of completions.
match :: (String -> Bool) -> (String -> IO [String]) -> Completer
match p suggest ts = case ts of
    []     -> []
    [t]    -> [Suggestions $ suggest t]
    (t:ts) -> if p t then continue ts else []

escape :: String -> String
escape = concatMap escape'
    where escape' ':' = "\\:"
          escape' c = [c]

-- Return words 
matchesFrom :: [String] -> String -> [String]
matchesFrom xs t = [(escape x) ++ " " | x <- xs, t `isPrefixOf` x]

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
