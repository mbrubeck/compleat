import Data.List (isPrefixOf)

type Completer = String -> [String]

token :: String -> Completer
token x y | y `isPrefixOf` x  = [x]
          | otherwise         = []

(<|>) :: Completer -> Completer -> Completer
a <|> b = \x -> a x ++ b x
