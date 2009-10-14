module Usage () where

import Text.Parsec.Char (char, noneOf, string, spaces)
import Text.Parsec.Combinator (anyToken, option, between)
import Text.Parsec.Prim ((<|>), many)
import Text.Parsec.String (Parser)
import qualified Completer as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellStyle)

terms :: Parser C.Completer
terms = many term >>=
    \cs -> return (foldl (C.-->) C.continue cs)

term :: Parser C.Completer
term = str <|> group <|> optionGroup

group :: Parser C.Completer
group = between (char '(') (char ')') terms

optionGroup :: Parser C.Completer
optionGroup = between (char '[') (char ']') terms

str :: Parser C.Completer
str = do
    p <- between (char '"') (char '"') (many $ noneOf "\"")
    return (C.str p)

token :: Parser a -> Parser a
token p = do
    result <- p
    spaces
    return result

-- Lexer?


lexer :: T.TokenParser ()
lexer  = T.makeTokenParser haskellStyle

whiteSpace = T.whiteSpace lexer
lexeme     = T.lexeme lexer
symbol     = T.symbol lexer
natural    = T.natural lexer
parens     = T.parens lexer
semi       = T.semi lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer
reservedOp = T.reservedOp lexer
