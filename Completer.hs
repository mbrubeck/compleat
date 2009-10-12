data Completion = Tokens [String] | Suggestions [String]
type Completer = [String] -> [Completion]

str :: String -> Completer
str s = match (s ==) (const $ Suggestions [s])

match :: (String -> Bool) -> (String -> Completion) -> Completer
match p suggest ts = case ts of
    []     -> []
    [t]    -> if p t then [suggest t] else []
    (t:ts) -> if p t then [Tokens ts] else []
