import Data.List (isPrefixOf)

type Completer = [String] -> [([String], String)]

token :: String -> Completer
token t [x]    | x `isPrefixOf` t  = [([], t)]
token t (x:xs) | x == t            = [(xs, t)]
token t _                          = []

(<|>) :: Completer -> Completer -> Completer
a <|> b = \x -> a x ++ b x

(=>>) :: Completer -> Completer -> Completer
a =>> b = concat . map (b . fst) . a
