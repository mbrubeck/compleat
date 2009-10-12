data Completion = Tokens [String] | Suggestions [String]
type Completer = [String] -> [Completion]

(<|>) :: Completer -> Completer -> Completer
c <|> d = \ts -> c ts ++ d ts

(-->) :: Completer -> Completer -> Completer
c --> d = \ts -> concat [ case result of
                            Tokens ts' -> d ts'
                            _          -> [result]
                        | result <- c ts]

str :: String -> Completer
str s = match (s ==) (const $ Suggestions [s])

match :: (String -> Bool) -> (String -> Completion) -> Completer
match p suggest ts = case ts of
    []     -> []
    [t]    -> if p t then [suggest t] else []
    (t:ts) -> if p t then [Tokens ts] else []
