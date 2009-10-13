module Usage () where

import Text.Parsec.Char (char, noneOf, string)
import Text.Parsec.Combinator (anyToken, option)
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
group = do
    char '('
    c <- terms 
    char ')'
    return c

optionGroup :: Parser C.Completer
optionGroup = do
    char '['
    c <- terms 
    char ']'
    return (c C.<|> C.continue)

str :: Parser C.Completer
str = do
    char '"'
    s <- many (noneOf "\"")
    char '"'
    return (C.str s)

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
