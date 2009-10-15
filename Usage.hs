module Usage () where

import Text.Parsec.Combinator (anyToken, chainl1)
import Text.Parsec.Prim ((<|>), many, try, runParser)
import Text.Parsec.String (Parser)
import qualified Completer as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellStyle)

terms :: Parser C.Completer
terms = do
    cs <- many term
    return (foldl (C.-->) C.continue cs)

term :: Parser C.Completer
term = repeated (group <|> str) C.many1 id
   <|> repeated optionGroup C.many C.optional

choice :: Parser C.Completer
choice = chainl1 term (symbol "|" >> return (C.<|>))

group :: Parser C.Completer
group = parens item

optionGroup :: Parser C.Completer
optionGroup = brackets item

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = do
    c <- p
    try (symbol "..." >> return (f c)) <|> return (g c)

item :: Parser C.Completer
item = try terms <|> choice

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
