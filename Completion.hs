import Data.List (isPrefixOf)

type Completer = [String] -> [(String, [String])]

(<|>) :: Completer -> Completer -> Completer
a <|> b = \x -> a x ++ b x

bind :: Completer -> (String -> Completer) -> Completer
a `bind` f = \inp -> concat [f st inp' | (st, inp') <- a inp]

(=>>) :: Completer -> Completer -> Completer
a =>> b = a `bind` \_ -> b

zero :: Completer
zero _ = []

result :: String -> Completer
result x inp = [(x,inp)]

item :: Completer
item []     = []
item (x:xs) = [(x,xs)]

sat :: (String -> Bool) -> Completer
sat p = item `bind` \x ->
        if p x then result x else zero

token :: String -> Completer
token t [x]    | x `isPrefixOf` t  = [(t, [])]
token t (x:xs) | x == t            = [(x, xs)]
token t _                          = []


