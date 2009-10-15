module Usage () where

import Text.Parsec.Char (char, noneOf, string, spaces)
import Text.Parsec.Combinator (anyToken, option, between)
import Text.Parsec.Prim ((<|>), many)
import Text.Parsec.String (Parser)
import qualified Completer as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellStyle)

terms :: Parser C.Completer
terms = do
    cs <- many term
    return (foldl (C.-->) C.continue cs)

term :: Parser C.Completer
term = group <|> optionGroup <|> str

repeater = do
    c <- term
    symbol "..."
    return (C.many1 c)

choice = do
    c <- term
    symbol "|"
    d <- term
    return (c C.<|> d)

group :: Parser C.Completer
group = parens terms

optionGroup :: Parser C.Completer
optionGroup = do
    c <- brackets terms
    return (c C.<|> C.continue)

str :: Parser C.Completer
str = do
    s <- stringLiteral <|> identifier
    return (C.str s)

-- Lexer?

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser haskellStyle

whiteSpace    = T.whiteSpace lexer
lexeme        = T.lexeme lexer
symbol        = T.symbol lexer
natural       = T.natural lexer
parens        = T.parens lexer
brackets      = T.parens lexer
semi          = T.semi lexer
identifier    = T.identifier lexer
reserved      = T.reserved lexer
reservedOp    = T.reservedOp lexer
stringLiteral = T.stringLiteral lexer
