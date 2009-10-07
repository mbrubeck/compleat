import Data.List (isPrefixOf)

type Completer = [String] -> [(String, [String])]

zero :: Completer
zero _ = []

item :: Completer
item []     = []
item (x:xs) = [(x,xs)]

token :: String -> Completer
token t [x]    | x `isPrefixOf` t  = [(x, [])]
token t (x:xs) | x == t            = [(x, xs)]
token t _                          = []

(<|>) :: Completer -> Completer -> Completer
a <|> b = \x -> a x ++ b x

(=>>) :: Completer -> Completer -> Completer
a =>> b = concat . map (b . snd) . a

