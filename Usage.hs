module Usage () where

import Text.Parsec.Combinator (anyToken, chainl1)
import Text.Parsec.Prim ((<|>), many, try, runParser)
import Text.Parsec.String (Parser)
import qualified Completer as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)

terms :: Parser C.Completer
terms = do
    cs <- many term
    return (foldl (C.-->) C.continue cs)

term = repeated (choiceGroup <|> str) C.many1 id
   <|> repeated optionGroup C.many C.optional

optionGroup = do
    c <- brackets (try choice <|> terms)
    return c

choiceGroup = parens choice

choice = do
    c <- term
    symbol "|"
    d <- (try choice <|> term)
    return (c C.<|> d)

str = do
    s <- stringLiteral <|> identifier
    return (C.str s)

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = do
    c <- p
    try (symbol "..." >> return (f c)) <|> return (g c)

-- Lexer

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser javaStyle

whiteSpace    = T.whiteSpace lexer
lexeme        = T.lexeme lexer
symbol        = T.symbol lexer
natural       = T.natural lexer
parens        = T.parens lexer
brackets      = T.brackets lexer
semi          = T.semi lexer
identifier    = T.identifier lexer
reserved      = T.reserved lexer
reservedOp    = T.reservedOp lexer
stringLiteral = T.stringLiteral lexer
